package ru.ifmo.fbsat.task.extendedmince

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.task.extendedce.ExtendedCETask
import ru.ifmo.fbsat.task.extendedmin.ExtendedMinTask
import java.io.File

class ExtendedMinCETask(
    val scenarioTree: ScenarioTree,
    val initialNegativeScenarioTree: NegativeScenarioTree?,
    val numberOfStates: Int?, // C
    val maxOutgoingTransitions: Int?, // K, K=C if null
    val maxGuardSize: Int, // P
    val initialMaxTotalGuardsSize: Int? = null, // N_init, unconstrained if null
    val solverProvider: () -> Solver,
    val smvDir: File,
    private val isEncodeAutomaton: Boolean = false,
    private val isEncodeTransitionsOrder: Boolean = false
) {
    private var _executed = false

    @Suppress("LocalVariableName")
    fun infer(): Automaton? {
        check(!_executed) { "The task has already been executed" }
        _executed = true

        val negativeScenarioTree = initialNegativeScenarioTree
            ?: NegativeScenarioTree.empty(scenarioTree.inputNames, scenarioTree.outputNames)

        val taskExtMin = ExtendedMinTask(
            scenarioTree,
            negativeScenarioTree,
            numberOfStates,
            maxOutgoingTransitions,
            maxGuardSize,
            initialMaxTotalGuardsSize,
            solverProvider,
            isEncodeAutomaton = isEncodeAutomaton,
            isEncodeTransitionsOrder = isEncodeTransitionsOrder
        )
        val automatonExtMin = taskExtMin.infer() ?: error("ExtendedMinTask could not infer an automaton")
        val C = automatonExtMin.numberOfStates
        val P = maxGuardSize
        // var N = automatonExtMin.totalGuardsSize
        var N = automatonExtMin.getN()

        println("[*] automatonExtMin:")
        automatonExtMin.pprint()
        println("[*] automatonExtMin has C = $C, P = $P, N = $N")

        out@ for (loopNumber in 1..100) {
            println("===== Loop number #$loopNumber =====")

            val task = ExtendedCETask(
                scenarioTree,
                negativeScenarioTree,
                C,
                maxOutgoingTransitions,
                P,
                solverProvider,
                smvDir = smvDir,
                isEncodeAutomaton = isEncodeAutomaton,
                isEncodeTransitionsOrder = isEncodeTransitionsOrder
            )

            val automaton = task.infer(N)
            if (automaton != null) {
                println("Hooray! Minimal full verified automaton has been found!")
                return automaton
            } else {
                println("UNSAT, N = $N is too small, trying larger value...")
                N += 1
            }
        }

        return null
    }
}
