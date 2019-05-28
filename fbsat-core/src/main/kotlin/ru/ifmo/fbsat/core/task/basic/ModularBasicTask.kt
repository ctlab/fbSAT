package ru.ifmo.fbsat.core.task.basic

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.automaton.ModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.core.solver.declareTotalizer
import ru.ifmo.fbsat.core.task.declareModularAlgorithmConstraints
import ru.ifmo.fbsat.core.task.declareModularColorConstraints
import ru.ifmo.fbsat.core.task.declareModularFiringConstraints
import ru.ifmo.fbsat.core.task.declareModularOutputEventConstraints
import ru.ifmo.fbsat.core.task.declareModularTransitionConstraints
import ru.ifmo.fbsat.core.utils.log
import java.io.File

interface ModularBasicTask {
    val scenarioTree: ScenarioTree
    val numberOfModules: Int // M
    val numberOfStates: Int // C
    val maxOutgoingTransitions: Int // K
    val maxTransitions: Int? // T, unconstrained if null
    val outDir: File

    fun infer(): ModularAutomaton?
    fun reuse(newMaxTransitions: Int): ModularBasicTask
    fun finalize2()

    companion object {
        @JvmStatic
        fun create(
            scenarioTree: ScenarioTree,
            numberOfModules: Int, // M
            numberOfStates: Int, // C
            maxOutgoingTransitions: Int? = null, // K, K=C if null
            maxTransitions: Int? = null, // T, unconstrained if null
            outDir: File,
            solverProvider: () -> Solver,
            autoFinalize: Boolean = true
        ): ModularBasicTask = ModularBasicTaskImpl(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions ?: numberOfStates,
            maxTransitions = maxTransitions,
            outDir = outDir,
            solver = solverProvider(),
            autoFinalize = autoFinalize
        )
    }
}

private class ModularBasicTaskImpl(
    override val scenarioTree: ScenarioTree,
    override val numberOfModules: Int, // M
    override val numberOfStates: Int, // C
    override val maxOutgoingTransitions: Int, // K
    override val maxTransitions: Int?, // T, unconstrained if null
    override val outDir: File,
    private val solver: Solver,
    private val autoFinalize: Boolean
) : ModularBasicTask {
    private var isExecuted = false
    private var isReused = false
    private var isFinalized = false

    init {
        with(solver) {
            declareModularBasicReduction()
            declareCardinality()
        }
    }

    override fun infer(): ModularAutomaton? {
        check(!isExecuted) { "This task has already been executed. Try using the `reuse()` method." }
        check(!isReused) { "This task has already been reused and can't be executed." }
        check(!isFinalized) { "This task has already been finalized and can't be executed." }
        isExecuted = true

        val rawAssignment = solver.solve()
        if (autoFinalize) finalize2()
        if (rawAssignment == null)
            return null

        val assignment = ModularBasicAssignment.fromRaw(rawAssignment)
        @Suppress("UnnecessaryVariable")
        val automaton = assignment.toAutomaton()
        return automaton
    }

    override fun reuse(newMaxTransitions: Int): ModularBasicTask {
        check(!isReused) { "This task has already been reused." }
        check(!isFinalized) { "This task has already been finalized and can't be reused." }
        if (!isExecuted)
            log.warn("Reusing the task that has not been executed yet.")
        isReused = true

        return ModularBasicTaskImpl(
            scenarioTree = this.scenarioTree,
            numberOfModules = this.numberOfModules,
            numberOfStates = this.numberOfStates,
            maxOutgoingTransitions = this.maxOutgoingTransitions,
            maxTransitions = newMaxTransitions,
            outDir = this.outDir,
            solver = this.solver,
            autoFinalize = this.autoFinalize
        )
    }

    override fun finalize2() {
        check(!isFinalized) { "This task has already been finalized." }
        isFinalized = true
        solver.finalize2()
    }

    @Suppress("LocalVariableName", "UNUSED_VARIABLE")
    private fun Solver.declareModularBasicReduction() {
        when (context["_isModularBasicReductionDeclared"] as Boolean?) {
            true -> return
            false -> error { "_isModularBasicReductionDeclared is false" }
            null -> context["_isModularBasicReductionDeclared"] = true
        }

        // Constants
        val scenarioTree by context(scenarioTree)
        val M by context(numberOfModules)
        val C by context(numberOfStates)
        val K by context(maxOutgoingTransitions)
        val V by context(scenarioTree.size)
        val E by context(scenarioTree.inputEvents.size)
        val O by context(scenarioTree.outputEvents.size)
        val X by context(scenarioTree.inputNames.size)
        val Z by context(scenarioTree.outputNames.size)
        val U by context(scenarioTree.uniqueInputs.size)

        // Variables
        val transition by context(newArray(M, C, K, C + 1))
        val actualTransition by context(newArray(M, C, E, U, C + 1))
        val inputEvent by context(newArray(M, C, K, E + 1))
        val outputEvent by context(newArray(M, C, O + 1))
        val outputVariableModule by context(newArray(Z, M))
        val algorithm0 by context(newArray(M, C, Z))
        val algorithm1 by context(newArray(M, C, Z))
        val color by context(newArray(M, V, C))
        val rootValue by context(newArray(M, C, K, U))
        val firstFired by context(newArray(M, C, U, K + 1))
        val notFired by context(newArray(M, C, U, K))

        // Constraints
        declareModularColorConstraints()
        declareModularTransitionConstraints()
        declareModularFiringConstraints()
        declareModularOutputEventConstraints()
        declareModularAlgorithmConstraints()
        // declareModularAutomatonBfsConstraints()
        // if (Globals.IS_ENCODE_TRANSITIONS_ORDER)
        //     declareModularTransitionsOrderConstraints()
        // TODO: declareAdhocConstraints()
    }

    @Suppress("LocalVariableName")
    private fun Solver.declareCardinality() {
        if (maxTransitions == null) return

        val T: Int by context(maxTransitions)
        val totalizer: IntArray = context.computeIfAbsent("_totalizerBasic") {
            val transition: IntMultiArray by context
            declareTotalizer(sequence {
                val (M, C, K, _) = transition.shape
                for (m in 1..M)
                    for (c in 1..C)
                        for (k in 1..K)
                            yield(-transition[m, c, k, C + 1])
            })
        } as IntArray
        val declaredT: Int? = context["_declaredT"] as Int?
        solver.declareComparatorLessThanOrEqual(totalizer, T, declaredT)
        context["_declaredT"] = T
    }
}
