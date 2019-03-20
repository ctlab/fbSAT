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

        // phase 1: ext-min to find C and N (P is given)
        // in loop:
        //    phase 2: ext-ce with C, P, N
        //     -> SAT => found automaton is minimal and fully verified
        //     -> UNSAT => N is too small, repeat with larger N (+1)
        //
        // Variation of phase 2:
        //    ext-ce with C, P, without N
        //      -> if UNSAT then C or P are too small, repeat with larger P up to UB, then try increasing C
        //      -> if SAT then incrementally call ext-ce with C, P, N
        //         -> SAT/UNSAT => same as above

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
        // val taskExtMin = ExtendedTask(
        //     scenarioTree,
        //     negativeScenarioTree,
        //     numberOfStates!!,
        //     maxOutgoingTransitions,
        //     maxGuardSize,
        //     solverProvider,
        //     isEncodeAutomaton = isEncodeAutomaton,
        //     isEncodeTransitionsOrder = isEncodeTransitionsOrder
        // )
        // val automatonExtMin =
        //     taskExtMin.infer(initialMaxTotalGuardsSize) ?: error("ExtendedMinTask could not infer an automaton")
        val C = automatonExtMin.numberOfStates
        val P = maxGuardSize
        var N = automatonExtMin.totalGuardsSize
        // val initialAlgorithmsAssumptions = automatonExtMin.getAlgorithmsAssumptions()

        println("C = $C, P = $P, N = $N")

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
                isEncodeTransitionsOrder = isEncodeTransitionsOrder //,
                // initialAlgorithmsAssumptions = initialAlgorithmsAssumptions
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
