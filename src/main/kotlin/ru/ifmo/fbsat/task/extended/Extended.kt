package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import kotlin.system.measureTimeMillis

// FIXME: maybe rename to `AutomatonInferrerExtended` or `ExtendedAutomatonInferenceTask`
class Extended(
    val scenarioTree: ScenarioTree,
    val negativeScenarioTree: NegativeScenarioTree?,
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int?, // K, K=C if null
    val maxGuardSize: Int, // P
    val solverProvider: () -> Solver,
    private val isForbidLoops: Boolean = true
) {
    private val solver = solverProvider()
    private var baseReduction: Reduction? = null
    private var totalizer: IntArray? = null
    private var declaredMaxTotalGuardSize: Int? = null
    private var ceReduction: CEReduction? = null

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
            val assignment = Assignment.fromRaw(rawAssignment, baseReduction!!)
            if (ceReduction != null) {
                @Suppress("UNUSED_VARIABLE")
                val ceAssignment = CEAssignment.fromRaw(rawAssignment, ceReduction!!)
            }
            val automaton = assignment.toAutomaton()
            automaton
        } else null
    }

    fun finalize() {
        solver.finalize()
    }

    private fun declareBaseReduction() {
        if (baseReduction == null) {
            measureTimeMillis {
                baseReduction = solver.declareBaseReductionExtended(
                    scenarioTree,
                    C = numberOfStates,
                    K = maxOutgoingTransitions ?: numberOfStates,
                    P = maxGuardSize
                )
            }.also {
                println(
                    "[+] Done declaring base reduction (${solver.numberOfVariables} variables, ${solver.numberOfClauses} clauses) in %.3f seconds"
                        .format(it / 1000.0)
                )
            }
        }
    }

    private fun declareCardinality(maxTotalGuardSize: Int?) {
        require(baseReduction != null) { "Run declareBaseReductionExtended first" }

        if (maxTotalGuardSize != null) {
            if (totalizer == null) {
                totalizer = solver.declareTotalizerExtended(baseReduction!!)
            }
            solver.declareComparatorExtended(totalizer!!, maxTotalGuardSize, declaredMaxTotalGuardSize)
            declaredMaxTotalGuardSize = maxTotalGuardSize
        }
    }

    private fun declareCE() {
        if (negativeScenarioTree == null) return
        if (negativeScenarioTree.counterExamples.isEmpty()) return
        // FIXME: must do following:
        if (negativeScenarioTree.counterExamples.first().elements.isEmpty()) return
        if (ceReduction != null) return

        val runningTime = measureTimeMillis {
            ceReduction = solver.declareCounterExampleExtended(
                baseReduction!!,
                ceReduction,
                scenarioTree,
                negativeScenarioTree,
                isForbidLoops = isForbidLoops
            )
        }
        println(
            "[+] Done declaring CE reduction (${solver.numberOfVariables} variables, ${solver.numberOfClauses} clauses) in %.3f seconds"
                .format(runningTime / 1000.0)
        )
    }
}
