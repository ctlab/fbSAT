package ru.ifmo.fbsat.core.task.extended

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.Solver.Companion.falseVariable
import ru.ifmo.fbsat.core.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.core.solver.declareTotalizer
import ru.ifmo.fbsat.core.task.basic.BasicTask
import ru.ifmo.fbsat.core.task.declareAndOrNodesConstraints
import ru.ifmo.fbsat.core.task.declareGuardBfsConstraints
import ru.ifmo.fbsat.core.task.declareNodeTypeConstraints
import ru.ifmo.fbsat.core.task.declareNoneTypeNodesConstraints
import ru.ifmo.fbsat.core.task.declareNotNodesConstraints
import ru.ifmo.fbsat.core.task.declareParentAndChildrenConstraints
import ru.ifmo.fbsat.core.task.declareTerminalsConstraints
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
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
            isEncodeReverseImplication = isEncodeReverseImplication,
            basicTask = null
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
    private val isEncodeReverseImplication: Boolean,
    basicTask: BasicTask?
) : ExtendedTask {
    @Suppress("JoinDeclarationAndAssignment")
    private val basicTask: BasicTask
    private var isExecuted = false
    private var isReused = false
    private var isFinalized = false

    init {
        this.basicTask = basicTask ?: BasicTask.create(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = null,
            outDir = outDir,
            solverProvider = { solver },
            autoFinalize = false,
            isEncodeReverseImplication = isEncodeReverseImplication
        )

        with(solver) {
            declareExtendedReduction()
            declareCardinality()
        }
    }

    @Suppress("LocalVariableName", "UNUSED_VARIABLE")
    private fun Solver.declareExtendedReduction() {
        when (context["_isExtendedReductionDeclared"] as Boolean?) {
            true -> return
            false -> error { "_isExtendedReductionDeclared is false" }
            null -> context["_isExtendedReductionDeclared"] = true
        }

        // Constants
        val C: Int by context
        val K: Int by context
        val P: Int by context(maxGuardSize)
        val X: Int by context
        val U: Int by context

        // Variables
        val nodeType: IntMultiArray by context(
            newArray(C, K, P, NodeType.values().size) { (_, _, _, nt) ->
                if (Globals.IS_FORBID_OR && NodeType.from(nt - 1) == NodeType.OR) falseVariable else newVariable()
            }
        )
        val terminal: IntMultiArray by context(newArray(C, K, P, X + 1))
        val parent: IntMultiArray by context(newArray(C, K, P, P + 1) { (_, _, p, par) ->
            if (par < p || par == P + 1) newVariable() else falseVariable
        })
        val child: IntMultiArray by context(newArray(C, K, P, P + 1) { (_, _, p, ch) ->
            if (ch >= p + 1 || ch == P + 1) newVariable() else falseVariable
        })
        val rootValue: IntMultiArray by context
        val nodeValue: IntMultiArray by context(newArray(C, K, P, U) { (c, k, p, u) ->
            if (p == 1) rootValue[c, k, u] else newVariable()
        })

        // Constraints
        declareNodeTypeConstraints()
        declareParentAndChildrenConstraints()
        declareNoneTypeNodesConstraints()
        declareTerminalsConstraints()
        declareAndOrNodesConstraints()
        declareNotNodesConstraints()
        if (Globals.IS_BFS_GUARD)
            declareGuardBfsConstraints()
        // TODO: declareAdhocConstraints()
        declareAdhocConstraints()
    }

    @Suppress("LocalVariableName")
    private fun Solver.declareCardinality() {
        if (maxTotalGuardsSize == null && !Globals.IS_ENCODE_TOTALIZER) return

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

        if (maxTotalGuardsSize == null) return

        val N: Int by context(maxTotalGuardsSize)
        val declaredN: Int? = context["_declaredN"] as Int?
        solver.declareComparatorLessThanOrEqual(totalizer, N, declaredN)
        context["_declaredN"] = N
    }

    private fun Solver.declareAdhocConstraints() {
        val C: Int by context
        val K: Int by context
        val P: Int by context
        val nodeType: IntMultiArray by context
        val child: IntMultiArray by context

        comment("A. AD-HOCs")

        comment("A.1. Forbid double negation")
        // nodetype[p, NOT] & child_left[p, ch] => ~nodetype[ch, NOT]
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1 until P)
                    for (ch in (p + 1)..P)
                        clause(
                            -nodeType[c, k, p, NodeType.NOT.value],
                            -child[c, k, p, ch],
                            -nodeType[c, k, ch, NodeType.NOT.value]
                        )

        comment("A.2. Distinct transitions")
        // TODO: Distinct transitions

        if (Globals.IS_FORBID_OR) {
            comment("A.3. Forbid ORs")
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        clause(-nodeType[c, k, p, NodeType.OR.value])
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
            isEncodeReverseImplication = this.isEncodeReverseImplication,
            basicTask = this.basicTask
        )
    }

    override fun finalize2() {
        check(!isFinalized) { "This task has already been finalized." }
        isFinalized = true
        basicTask.finalize2()
        // Note: basicTask already finalizes solver, so it is not necessary to do it here again
        // solver.finalize2()
    }
}
