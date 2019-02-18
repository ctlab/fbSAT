package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.solver.declareComparatorLessThanOrEqual
import kotlin.system.measureTimeMillis

// FIXME: maybe rename to `AutomatonInferrerExtended` or `ExtendedAutomatonInferenceTask`
class Extended(
    val scenarioTree: ScenarioTree,
    val negativeScenarioTree: NegativeScenarioTree?,
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int?, // K, K=C if null
    val maxGuardSize: Int, // P
    solverProvider: () -> Solver,
    private val isForbidLoops: Boolean = true
) {
    private val solver = solverProvider()
    private var baseReduction: BaseReduction? = null
    private var totalizer: IntArray? = null
    private var declaredMaxTotalGuardSize: Int? = null
    private var negativeReduction: NegativeReduction? = null

    /**
     * @param[maxTotalGuardsSize] maximum total guard size *N*, unconstrained if `null`
     * @return automaton if *SAT*, or `null` if *UNSAT*
     */
    fun infer(maxTotalGuardsSize: Int? = null, finalize: Boolean = true): Automaton? {
        declareBaseReduction()
        declareCardinality(maxTotalGuardsSize)
        declareCE()

        solver.comment("Total variables: ${solver.numberOfVariables}, clauses: ${solver.numberOfClauses}")

        val rawAssignment = solver.solve()
        if (finalize) finalize()

        return if (rawAssignment != null) {
            val assignment = BaseAssignment.fromRaw(rawAssignment, baseReduction!!)
            if (negativeReduction != null) {
                @Suppress("UNUSED_VARIABLE")
                val ceAssignment = NegativeAssignment.fromRaw(rawAssignment, negativeReduction!!)
            }
            val automaton = assignment.toAutomaton()
            automaton
        } else null
    }

    fun finalize() {
        solver.finalize()
    }

    private fun declareBaseReduction() {
        if (baseReduction != null) return

        measureTimeMillis {
            baseReduction = BaseReduction(
                scenarioTree,
                C = numberOfStates,
                K = maxOutgoingTransitions ?: numberOfStates,
                P = maxGuardSize,
                solver = solver
            )
        }.also {
            println(
                "[+] Done declaring base reduction (${solver.numberOfVariables} variables, ${solver.numberOfClauses} clauses) in %.3f seconds"
                    .format(it / 1000.0)
            )
        }
    }

    private fun declareCardinality(maxTotalGuardSize: Int?) {
        require(baseReduction != null) { "Run declareBaseReductionExtended first" }

        if (maxTotalGuardSize != null) {
            if (totalizer == null) {
                totalizer = solver.declareTotalizerExtended(baseReduction!!)
            }
            solver.declareComparatorLessThanOrEqual(totalizer!!, maxTotalGuardSize, declaredMaxTotalGuardSize)
            declaredMaxTotalGuardSize = maxTotalGuardSize
        }
    }

    private fun declareCE() {
        if (negativeScenarioTree == null) return
        if (negativeScenarioTree.counterExamples.isEmpty()) return
        // FIXME: must do following:
        if (negativeScenarioTree.counterExamples.first().elements.isEmpty()) return
        if (negativeReduction != null) return

        val nov = solver.numberOfVariables
        val noc = solver.numberOfClauses
        val runningTime = measureTimeMillis {
            negativeReduction = NegativeReduction(
                scenarioTree,
                baseReduction!!,
                negativeScenarioTree,
                negativeReduction,
                solver,
                isForbidLoops = isForbidLoops
            )
        }
        println(
            "[+] Done declaring CE reduction (${solver.numberOfVariables - nov} variables, ${solver.numberOfClauses - noc} clauses) in %.3f seconds"
                .format(runningTime / 1000.0)
        )
    }
}
