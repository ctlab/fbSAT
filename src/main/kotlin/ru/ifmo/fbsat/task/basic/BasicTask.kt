package ru.ifmo.fbsat.task.basic

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.solver.declareComparatorLessThanOrEqual
import kotlin.system.measureTimeMillis

class BasicTask(
    val scenarioTree: ScenarioTree,
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int?, // K, K=C if null
    solverProvider: () -> Solver,
    val isEncodeTransitionsOrder: Boolean
) {
    private val solver = solverProvider()
    private var baseReduction: BaseReduction? = null
    private var totalizer: IntArray? = null
    private var declaredMaxTransitions: Int? = null

    /**
     * Infer automaton using **basic** method.
     *
     * @param[maxTransitions] maximum number of transitions *T*, unconstrained if `null`.
     * @param[finalize] call `finalize` method after inference?
     * @return automaton if *SAT*, or `null` if *UNSAT*.
     */
    fun infer(maxTransitions: Int? = null, finalize: Boolean = true): Automaton? {
        declareBaseReduction()
        declareCardinality(maxTransitions)

        solver.comment("Total variables: ${solver.numberOfVariables}, clauses: ${solver.numberOfClauses}")

        val rawAssignment = solver.solve()
        if (finalize) finalize()

        return if (rawAssignment != null) {
            val assignment = BaseAssignment.fromRaw(rawAssignment, baseReduction!!)
            @Suppress("UnnecessaryVariable")
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
                solver = solver,
                isEncodeTransitionsOrder = isEncodeTransitionsOrder
            )
        }.also {
            println(
                "[+] Done declaring base reduction (${solver.numberOfVariables} variables, ${solver.numberOfClauses} clauses) in %.3f seconds"
                    .format(it / 1000.0)
            )
        }
    }

    private fun declareCardinality(maxTransitions: Int?) {
        if (maxTransitions != null) {
            if (totalizer == null) {
                totalizer = solver.declareTotalizer(baseReduction!!)
            }

            solver.declareComparatorLessThanOrEqual(totalizer!!, maxTransitions, declaredMaxTransitions)
            declaredMaxTransitions = maxTransitions
        }
    }

    // private fun declareCE(negativeScenarioTree: NegativeScenarioTree) {
    //     if (!ceReduction) {
    //         solver.declareCE(
    //             baseReduction!!,
    //             negativeScenarioTree
    //         )
    //         ceReduction = true
    //     }
    // }
}
