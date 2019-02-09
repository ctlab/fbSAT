package ru.ifmo.fbsat.task.basic

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import kotlin.system.measureTimeMillis

// BasicAutomatonInferenceTask
class Basic(
    val scenarioTree: ScenarioTree,
    val negativeScenarioTree: NegativeScenarioTree?,
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int?, // K, K=C if null
    val solverProvider: () -> Solver
) {
    private val solver = solverProvider()
    private var baseReduction: Reduction? = null
    private var totalizer: IntArray? = null
    private var declaredMaxTransitions: Int? = null
    private var ceReduction: Boolean = false

    /**
     * @param[maxTransitions] maximum number of transitions *T*, unconstrained if `null`
     * @return automaton if *SAT*, or `null` if *UNSAT*
     */
    fun infer(maxTransitions: Int? = null, finalize: Boolean = true): Automaton? {
        declareBaseReduction()
        declareCardinality(maxTransitions)

        // =============================================
        // if (negativeScenarioTree != null)
        //     declareCE(negativeScenarioTree)
        // =============================================
        solver.comment("Total variables: ${solver.numberOfVariables}, clauses: ${solver.numberOfClauses}")

        val automaton = solver.solve()?.let { Assignment.fromRaw(baseReduction!!, it).toAutomaton() }

        if (finalize) finalize()

        return automaton
    }

    fun finalize() {
        solver.finalize()
    }

    private fun declareBaseReduction() {
        if (baseReduction == null) {
            measureTimeMillis {
                baseReduction = solver.declareBaseReduction(
                    scenarioTree,
                    C = numberOfStates,
                    K = maxOutgoingTransitions ?: numberOfStates
                )
            }.also {
                println(
                    "[+] Done declaring base reduction (${solver.numberOfVariables} variables, ${solver.numberOfClauses} clauses) in %.3f seconds"
                        .format(it / 1000.0)
                )
            }
        }
    }

    private fun declareCardinality(maxTransitions: Int?) {
        if (maxTransitions != null) {
            if (totalizer == null) {
                totalizer = solver.declareTotalizer(baseReduction!!)
            }
            solver.declareComparator(totalizer!!, maxTransitions, declaredMaxTransitions)
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
