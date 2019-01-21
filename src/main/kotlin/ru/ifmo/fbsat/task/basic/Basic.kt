package ru.ifmo.fbsat.task.basic

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.CounterExampleTree
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import kotlin.system.measureTimeMillis

// BasicAutomatonInferenceTask
class Basic(
    val scenarioTree: ScenarioTree,
    val counterExampleTree: CounterExampleTree?,
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int?, // K, K=C if null
    val solverProducer: () -> Solver
) {
    private val solver = solverProducer()
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
        // if (counterExampleTree != null)
        //     declareCE(counterExampleTree)
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
                baseReduction = Reduction.declareBaseReduction(
                    solver,
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
                totalizer = Reduction.declareTotalizer(solver, baseReduction!!)
            }
            Reduction.declareComparator(solver, totalizer!!, maxTransitions, declaredMaxTransitions)
            declaredMaxTransitions = maxTransitions
        }
    }

    private fun declareCE(counterExampleTree: CounterExampleTree) {
        if (!ceReduction) {
            Reduction.declareCE(
                solver,
                baseReduction!!,
                counterExampleTree
            )
            ceReduction = true
        }
    }
}
