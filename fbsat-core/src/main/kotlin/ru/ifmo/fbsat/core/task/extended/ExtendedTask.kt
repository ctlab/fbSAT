package ru.ifmo.fbsat.core.task.extended

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.core.solver.declareTotalizer
import ru.ifmo.fbsat.core.task.declareAlgorithmConstraints
import ru.ifmo.fbsat.core.task.declareAndOrNodesConstraints
import ru.ifmo.fbsat.core.task.declareAutomatonBfsConstraints
import ru.ifmo.fbsat.core.task.declareColorConstraints
import ru.ifmo.fbsat.core.task.declareFiringConstraints
import ru.ifmo.fbsat.core.task.declareGuardBfsConstraints
import ru.ifmo.fbsat.core.task.declareNodeTypeConstraints
import ru.ifmo.fbsat.core.task.declareNoneTypeNodesConstraints
import ru.ifmo.fbsat.core.task.declareNotNodesConstraints
import ru.ifmo.fbsat.core.task.declareOutputEventConstraints
import ru.ifmo.fbsat.core.task.declareParentAndChildrenConstraints
import ru.ifmo.fbsat.core.task.declareTerminalsConstraints
import ru.ifmo.fbsat.core.task.declareTransitionConstraints
import ru.ifmo.fbsat.core.task.declareTransitionsOrderConstraints
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.multiarray.IntMultiArray
import java.io.File

interface ExtendedTask {
    val scenarioTree: ScenarioTree
    val numberOfStates: Int // C
    val maxOutgoingTransitions: Int // K
    val maxGuardSize: Int // P
    val maxTotalGuardsSize: Int? // N, unconstrained if null
    val outDir: File

    fun infer(): Automaton?
    fun reuse(newMaxTotalGuardsSize: Int): ExtendedTask
    fun finalize2()

    companion object {
        @JvmStatic
        fun create(
            scenarioTree: ScenarioTree,
            numberOfStates: Int, // C
            maxOutgoingTransitions: Int? = null, // K, K=C if null
            maxGuardSize: Int, // P
            maxTotalGuardsSize: Int? = null, // N, unconstrained if null
            outDir: File,
            solverProvider: () -> Solver,
            autoFinalize: Boolean = true,
            isEncodeReverseImplication: Boolean = true
        ): ExtendedTask = ExtendedTaskImpl(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions ?: numberOfStates,
            maxGuardSize = maxGuardSize,
            maxTotalGuardsSize = maxTotalGuardsSize,
            outDir = outDir,
            solver = solverProvider(),
            autoFinalize = autoFinalize,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
    }
}

private class ExtendedTaskImpl(
    override val scenarioTree: ScenarioTree,
    override val numberOfStates: Int, // C
    override val maxOutgoingTransitions: Int, // K
    override val maxGuardSize: Int, // P
    override val maxTotalGuardsSize: Int?, // N, unconstrained if null
    override val outDir: File,
    private val solver: Solver,
    private val autoFinalize: Boolean,
    private val isEncodeReverseImplication: Boolean
) : ExtendedTask {
    private var isExecuted = false
    private var isReused = false
    private var isFinalized = false

    init {
        with(solver) {
            declareBaseReduction()
            declareCardinality()
        }
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

        val assignment = ExtendedAssignment.fromRaw(rawAssignment)
        @Suppress("UnnecessaryVariable")
        val automaton = assignment.toAutomaton()
        return automaton
    }

    override fun reuse(newMaxTotalGuardsSize: Int): ExtendedTask {
        check(!isReused) { "This task has already been reused." }
        check(!isFinalized) { "This task has already been finalized and can't be reused." }
        if (!isExecuted)
            log.warn("Reusing the task that has not been executed yet.")
        isReused = true

        return ExtendedTaskImpl(
            scenarioTree = this.scenarioTree,
            numberOfStates = this.numberOfStates,
            maxOutgoingTransitions = this.maxOutgoingTransitions,
            maxGuardSize = this.maxGuardSize,
            maxTotalGuardsSize = newMaxTotalGuardsSize,
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

    @Suppress("LocalVariableName", "UNUSED_VARIABLE")
    private fun Solver.declareBaseReduction() {
        when (context["_isBaseReductionDeclared"] as Boolean?) {
            true -> return
            false -> error { "_isBaseReductionDeclared is false" }
            null -> context["_isBaseReductionDeclared"] = true
        }

        // Constants
        context["scenarioTree"] = scenarioTree
        val C: Int by context(numberOfStates)
        val K: Int by context(maxOutgoingTransitions)
        val P: Int by context(maxGuardSize)
        val V: Int by context(scenarioTree.size)
        val E: Int by context(scenarioTree.inputEvents.size)
        val O: Int by context(scenarioTree.outputEvents.size)
        val X: Int by context(scenarioTree.inputNames.size)
        val Z: Int by context(scenarioTree.outputNames.size)
        val U: Int by context(scenarioTree.uniqueInputs.size)

        // Variables
        val falseVariable: Int by context
        val transition: IntMultiArray by context(newArray(C, K, C + 1))
        val actualTransition: IntMultiArray by context(newArray(C, E, U, C + 1))
        val inputEvent: IntMultiArray by context(newArray(C, K, E + 1))
        val outputEvent: IntMultiArray by context(newArray(C, O))
        val algorithm0: IntMultiArray by context(newArray(C, Z))
        val algorithm1: IntMultiArray by context(newArray(C, Z))
        val color: IntMultiArray by context(newArray(V, C))
        val nodeType: IntMultiArray by context(
            newArray(C, K, P, NodeType.values().size) { (_, _, _, nt) ->
                if (Globals.IS_FORBID_OR && NodeType.from(nt - 1) == NodeType.OR) falseVariable else newVariable()
            }
        )
        val terminal: IntMultiArray by context(newArray(C, K, P, X + 1))
        val parent: IntMultiArray by context(newArray(C, K, P, P + 1) { (_, _, p, par) ->
            if (par < p || par == P + 1) newVariable() else falseVariable
        })
        val childLeft: IntMultiArray by context(newArray(C, K, P, P + 1) { (_, _, p, ch) ->
            if (ch >= p + 1 || ch == P + 1) newVariable() else falseVariable
        })
        val childRight: IntMultiArray by context(newArray(C, K, P, P + 1) { (_, _, p, ch) ->
            if (ch >= p + 2 || ch == P + 1) newVariable() else falseVariable
        })
        val nodeValue: IntMultiArray by context(newArray(C, K, P, U))
        val rootValue: IntMultiArray by context(newArray(C, K, U) { (c, k, u) -> nodeValue[c, k, 1, u] })
        val childValueLeft: IntMultiArray by context(newArray(C, K, P, U))
        val childValueRight: IntMultiArray by context(newArray(C, K, P, U))
        val firstFired: IntMultiArray by context(newArray(C, U, K + 1))
        val notFired: IntMultiArray by context(newArray(C, U, K))

        // Constraints
        declareTransitionConstraints()
        declareFiringConstraints()
        declareOutputEventConstraints()
        declareAlgorithmConstraints()
        if (Globals.IS_BFS_AUTOMATON)
            declareAutomatonBfsConstraints()
        declareColorConstraints(isEncodeReverseImplication)
        declareNodeTypeConstraints()
        declareParentAndChildrenConstraints()
        declareNoneTypeNodesConstraints()
        declareTerminalsConstraints()
        declareAndOrNodesConstraints()
        declareNotNodesConstraints()
        if (Globals.IS_BFS_GUARD)
            declareGuardBfsConstraints()
        if (Globals.IS_ENCODE_TRANSITIONS_ORDER)
            declareTransitionsOrderConstraints()
        // TODO: declareAdhocConstraints()
    }

    @Suppress("LocalVariableName")
    private fun Solver.declareCardinality() {
        if (maxTotalGuardsSize == null) return

        val N: Int by context(maxTotalGuardsSize)
        val totalizer: IntArray = context.computeIfAbsent("_totalizerExtended") {
            val nodeType: IntMultiArray by context
            declareTotalizer {
                val (C, K, P, _) = nodeType.shape
                for (c in 1..C)
                    for (k in 1..K)
                        for (p in 1..P)
                            yield(-nodeType[c, k, p, NodeType.NONE.value])
            }
        } as IntArray
        val declaredN: Int? = context["_declaredN"] as Int?
        solver.declareComparatorLessThanOrEqual(totalizer, N, declaredN)
        context["_declaredN"] = N
    }

    //    private fun Solver.declareAdhocConstraints() {
    //         // val C: Int by context
    //         // val K: Int by context
    //         // val P: Int by context
    //         // val nodeType: IntMultiArray by context
    //         // val childLeft: IntMultiArray by context
    //
    //         comment("A. AD-HOCs")
    //
    //         comment("A.1. Forbid double negation")
    //         // nodetype[p, NOT] & child_left[p, ch] => ~nodetype[ch, NOT]
    //         for (c in 1..C)
    //             for (k in 1..K)
    //                 for (p in 1..(P - 1))
    //                     for (ch in (p + 1)..P)
    //                         clause(
    //                             -nodeType[c, k, p, NodeType.NOT.value],
    //                             -childLeft[c, k, p, ch],
    //                             -nodeType[c, k, ch, NodeType.NOT.value]
    //                         )
    //
    //         comment("A.2. Distinct transitions")
    //         // TODO: Distinct transitions
    //
    //         if (Globals.IS_FORBID_OR) {
    //             comment("A.3. Forbid ORs")
    //             for (c in 1..C)
    //                 for (k in 1..K)
    //                     for (p in 1..P)
    //                         clause(-nodeType[c, k, p, NodeType.OR.value])
    //         }
    //     }
}
