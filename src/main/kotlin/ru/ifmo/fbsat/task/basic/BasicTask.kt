package ru.ifmo.fbsat.task.basic

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.solver.declareComparatorLessThanOrEqual
import java.io.File

internal class Cardinality(
    val totalizer: IntArray,
    val declaredMaxTransitions: Int
)

class BasicTask private constructor(
    val scenarioTree: ScenarioTree,
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int, // K
    val maxTransitions: Int?, // T, unconstrained if null
    val outDir: File,
    private val solver: Solver,
    private val autoFinalize: Boolean,
    oldBaseReduction: BaseReduction?,
    oldCardinality: Cardinality?
) {
    private val baseReduction: BaseReduction
    private val cardinality: Cardinality?
    private var canBeReused: Boolean

    constructor(
        scenarioTree: ScenarioTree,
        numberOfStates: Int, // C
        maxOutgoingTransitions: Int? = null, // K, K=C if null
        maxTransitions: Int? = null, // T, unconstrained if null
        outDir: File,
        solver: Solver,
        autoFinalize: Boolean = true
    ) : this(
        scenarioTree = scenarioTree,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions ?: numberOfStates,
        maxTransitions = maxTransitions,
        outDir = outDir,
        solver = solver,
        autoFinalize = autoFinalize,
        oldBaseReduction = null,
        oldCardinality = null
    )

    init {
        this.baseReduction = oldBaseReduction ?: declareBaseReduction()
        this.cardinality = declareCardinality(maxTransitions, oldCardinality)
        this.canBeReused = !autoFinalize
    }

    fun infer(): Automaton? {
        val rawAssignment = solver.solve()
        if (autoFinalize) finalize2()

        if (rawAssignment == null)
            return null

        val assignment = BaseAssignment.fromRaw(rawAssignment, baseReduction)
        @Suppress("UnnecessaryVariable")
        val automaton = assignment.toAutomaton()
        return automaton
    }

    fun finalize2() {
        solver.finalize2()
        canBeReused = false
    }

    fun reuse(newMaxTransitions: Int): BasicTask {
        check(canBeReused)
        canBeReused = false

        return BasicTask(
            scenarioTree = this.scenarioTree,
            numberOfStates = this.numberOfStates,
            maxOutgoingTransitions = this.maxOutgoingTransitions,
            maxTransitions = newMaxTransitions,
            outDir = this.outDir,
            solver = this.solver,
            autoFinalize = this.autoFinalize,
            oldBaseReduction = this.baseReduction,
            oldCardinality = this.cardinality
        )
    }

    private fun declareBaseReduction(): BaseReduction {
        // TODO: ensure called no more than once

        return BaseReduction(
            scenarioTree = scenarioTree,
            C = numberOfStates,
            K = maxOutgoingTransitions,
            solver = solver
        )
    }

    private fun declareCardinality(maxTransitions: Int?, oldCardinality: Cardinality?): Cardinality? {
        // TODO: ensure called no more than once

        if (maxTransitions == null) return null

        val totalizer = oldCardinality?.totalizer ?: solver.declareTotalizer(baseReduction)
        val declaredMaxTransitions = oldCardinality?.declaredMaxTransitions
        solver.declareComparatorLessThanOrEqual(totalizer, maxTransitions, declaredMaxTransitions)

        return Cardinality(totalizer, maxTransitions)
    }
}
