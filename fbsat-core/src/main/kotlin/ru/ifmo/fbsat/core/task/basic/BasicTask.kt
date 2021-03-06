package ru.ifmo.fbsat.core.task.basic

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.core.solver.declareTotalizer
import ru.ifmo.fbsat.core.task.declareAlgorithmConstraints
import ru.ifmo.fbsat.core.task.declareAutomatonBfsConstraints
import ru.ifmo.fbsat.core.task.declareColorConstraints
import ru.ifmo.fbsat.core.task.declareFiringConstraints
import ru.ifmo.fbsat.core.task.declareOutputEventConstraints
import ru.ifmo.fbsat.core.task.declareTransitionConstraints
import ru.ifmo.fbsat.core.task.declareTransitionsOrderConstraints
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import java.io.File

interface BasicTask {
    val scenarioTree: ScenarioTree
    val numberOfStates: Int // C
    val maxOutgoingTransitions: Int // K
    val maxTransitions: Int? // T, unconstrained if null
    val outDir: File

    fun infer(): Automaton?
    fun reuse(newMaxTransitions: Int): BasicTask
    fun finalize2()

    companion object {
        @JvmStatic
        fun create(
            scenarioTree: ScenarioTree,
            numberOfStates: Int, // C
            maxOutgoingTransitions: Int? = null, // K, K=C if null
            maxTransitions: Int? = null, // T, unconstrained if null
            outDir: File,
            solverProvider: () -> Solver,
            autoFinalize: Boolean = true,
            isEncodeReverseImplication: Boolean = true
        ): BasicTask = BasicTaskImpl(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions ?: numberOfStates,
            maxTransitions = maxTransitions,
            outDir = outDir,
            solver = solverProvider(),
            autoFinalize = autoFinalize,
            isEncodeReverseImplication = isEncodeReverseImplication
        )

        // TODO: `createAsSubtask` factory method, which passes `isSubtask=true` to the constructor.
        // `isSubtask` flag prevents task from being used/reused (`.infer()`/`.reuse()`)
    }
}

private class BasicTaskImpl(
    override val scenarioTree: ScenarioTree,
    override val numberOfStates: Int, // C
    override val maxOutgoingTransitions: Int, // K
    override val maxTransitions: Int?, // T, unconstrained if null
    override val outDir: File,
    private val solver: Solver,
    private val autoFinalize: Boolean,
    private val isEncodeReverseImplication: Boolean
) : BasicTask {
    private var isExecuted = false
    private var isReused = false
    private var isFinalized = false

    init {
        with(solver) {
            declareBasicReduction()
            declareCardinality()
        }
    }

    @Suppress("LocalVariableName", "UNUSED_VARIABLE")
    private fun Solver.declareBasicReduction() {
        when (context["_isBasicReductionDeclared"] as Boolean?) {
            true -> return
            false -> error { "_isBasicReductionDeclared is false" }
            null -> context["_isBasicReductionDeclared"] = true
        }

        // Constants
        val scenarioTree: ScenarioTree by context(scenarioTree)
        val C: Int by context(numberOfStates)
        val K: Int by context(maxOutgoingTransitions)
        val V: Int by context(scenarioTree.size)
        val E: Int by context(scenarioTree.inputEvents.size)
        val O: Int by context(scenarioTree.outputEvents.size)
        val X: Int by context(scenarioTree.inputNames.size)
        val Z: Int by context(scenarioTree.outputNames.size)
        val U: Int by context(scenarioTree.uniqueInputs.size)

        // Variables
        val transition: IntMultiArray by context(newArray(C, K, C + 1))
        val actualTransition: IntMultiArray by context(newArray(C, E, U, C + 1))
        val inputEvent: IntMultiArray by context(newArray(C, K, E + 1))
        val outputEvent: IntMultiArray by context(newArray(C, O + 1))
        val algorithm0: IntMultiArray by context(newArray(C, Z))
        val algorithm1: IntMultiArray by context(newArray(C, Z))
        val color: IntMultiArray by context(newArray(V, C))
        val rootValue: IntMultiArray by context(newArray(C, K, U))
        val firstFired: IntMultiArray by context(newArray(C, U, K + 1))
        val notFired: IntMultiArray by context(newArray(C, U, K))

        // Constraints
        declareColorConstraints(isEncodeReverseImplication = isEncodeReverseImplication)
        declareTransitionConstraints()
        declareFiringConstraints()
        declareOutputEventConstraints()
        declareAlgorithmConstraints()
        if (Globals.IS_BFS_AUTOMATON)
            declareAutomatonBfsConstraints()
        if (Globals.IS_ENCODE_TRANSITIONS_ORDER)
            declareTransitionsOrderConstraints()
        // TODO: declareAdhocConstraints()
    }

    @Suppress("LocalVariableName")
    private fun Solver.declareCardinality() {
        if (maxTransitions == null && !Globals.IS_ENCODE_TOTALIZER) return

        val totalizer: IntArray = context.computeIfAbsent("_totalizerBasic") {
            val transition: IntMultiArray by context
            declareTotalizer(sequence {
                val (C, K, _) = transition.shape
                for (c in 1..C)
                    for (k in 1..K)
                        yield(-transition[c, k, C + 1])
            })
        } as IntArray

        if (maxTransitions == null) return

        val T: Int by context(maxTransitions)
        val declaredT: Int? = context["_declaredT"] as Int?
        solver.declareComparatorLessThanOrEqual(totalizer, T, declaredT)
        context["_declaredT"] = T
    }

    override fun infer(): Automaton? {
        check(!isExecuted) { "This task has already been executed. Try using the `reuse()` method." }
        check(!isReused) { "This task has already been reused and can't be executed." }
        check(!isFinalized) { "This task has already been finalized and can't be executed." }
        isExecuted = true

        val rawAssignment = solver.solve()
        if (autoFinalize) finalize2()
        if (rawAssignment == null)
            return null

        val assignment = BasicAssignment.fromRaw(rawAssignment)
        @Suppress("UnnecessaryVariable")
        val automaton = assignment.toAutomaton()
        return automaton
    }

    override fun reuse(newMaxTransitions: Int): BasicTask {
        check(!isReused) { "This task has already been reused." }
        check(!isFinalized) { "This task has already been finalized and can't be reused." }
        if (!isExecuted)
            log.warn("Reusing the task that has not been executed yet.")
        isReused = true

        return BasicTaskImpl(
            scenarioTree = this.scenarioTree,
            numberOfStates = this.numberOfStates,
            maxOutgoingTransitions = this.maxOutgoingTransitions,
            maxTransitions = newMaxTransitions,
            outDir = this.outDir,
            solver = this.solver,
            autoFinalize = this.autoFinalize,
            isEncodeReverseImplication = this.isEncodeReverseImplication
        )
    }

    override fun finalize2() {
        check(!isFinalized) { "This task has already been finalized." }
        isFinalized = true
        solver.finalize2()
    }
}
