package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.solver.declareComparatorLessThanOrEqual
import kotlin.system.measureTimeMillis

class ExtendedTask(
    val scenarioTree: ScenarioTree,
    val negativeScenarioTree: NegativeScenarioTree?,
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int?, // K, K=C if null
    val maxGuardSize: Int, // P
    solverProvider: () -> Solver,
    val isForbidLoops: Boolean = true,
    val isEncodeAutomaton: Boolean = false,
    val isEncodeTransitionsOrder: Boolean
) {
    private val solver: Solver = solverProvider()
    private var baseReduction: BaseReduction? = null
    private var totalizer: IntArray? = null
    private var declaredMaxTotalGuardSize: Int? = null
    private var negativeReduction: NegativeReduction? = null

    /**
     * Infer automaton using **extended** method.
     *
     * @param[maxTotalGuardsSize] maximum total guard size *N*, unconstrained if `null`.
     * @param[finalize] call `finalize` method after inference?
     * @return automaton if *SAT*, or `null` if *UNSAT*.
     */
    fun infer(
        maxTotalGuardsSize: Int? = null,
        finalize: Boolean = true
    ): Automaton? {
        declareBaseReduction()
        declareCardinality(maxTotalGuardsSize)
        declareNegativeReduction()

        solver.comment("Total variables: ${solver.numberOfVariables}, clauses: ${solver.numberOfClauses}")

        val rawAssignment = solver.solve()
        if (finalize) finalize()

        return if (rawAssignment != null) {
            val assignment = BaseAssignment.fromRaw(rawAssignment, baseReduction!!)
            val automaton = assignment.toAutomaton()
            if (negativeReduction != null) {
                val negativeAssignment = NegativeAssignment.fromRaw(rawAssignment, negativeReduction!!)
                automaton.checkNegativeAssignment(negativeAssignment, scenarioTree)
            }
            // ====================
            if (negativeScenarioTree != null) {
                check(automaton.verify(negativeScenarioTree)) { "NST verification failed" }
            }
            // ====================
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
                scenarioTree = scenarioTree,
                C = numberOfStates,
                K = maxOutgoingTransitions ?: numberOfStates,
                P = maxGuardSize,
                solver = solver,
                isEncodeAutomaton = isEncodeAutomaton,
                isEncodeTransitionsOrder = isEncodeTransitionsOrder
            )
        }.also {
            println(
                "[+] Done declaring base reduction (${solver.numberOfVariables} variables, ${solver.numberOfClauses} clauses) in %.3f seconds"
                    .format(it / 1000.0)
            )
        }
    }

    private fun declareCardinality(maxTotalGuardSize: Int?) {
        check(baseReduction != null) { "Run declareBaseReductionExtended first" }

        if (maxTotalGuardSize != null) {
            if (totalizer == null) {
                totalizer = solver.declareTotalizerExtended(baseReduction!!)
            }

            solver.declareComparatorLessThanOrEqual(totalizer!!, maxTotalGuardSize, declaredMaxTotalGuardSize)
            declaredMaxTotalGuardSize = maxTotalGuardSize
        }
    }

    private fun declareNegativeReduction() {
        if (negativeScenarioTree == null) return
        if (negativeScenarioTree.negativeScenarios.isEmpty()) return
        // FIXME: must do following:
        if (negativeScenarioTree.negativeScenarios.first().elements.isEmpty()) return

        // if (negativeReduction != null) return

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
            "[+] Done declaring negative reduction (${solver.numberOfVariables - nov} variables, ${solver.numberOfClauses - noc} clauses) in %.3f seconds"
                .format(runningTime / 1000.0)
        )
    }
}
