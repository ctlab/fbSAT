package ru.ifmo.fbsat.task.extendedmin

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.CounterExampleTree
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.task.basicmin.BasicMin
import ru.ifmo.fbsat.task.extended.Extended

class ExtendedMin(
        val scenarioTree: ScenarioTree,
        val counterExampleTree: CounterExampleTree?,
        val numberOfStates: Int?, // C, search if null
        val maxOutgoingTransitions: Int?, // K, =C if null
        val maxGuardSize: Int, // P
        val initialMaxTotalGuardsSize: Int?, // N_init, unconstrained if null
        val solverProducer: () -> Solver
) {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions == null)) {
            "do not specify only K"
        }
    }

    fun infer(): Automaton? {
        @Suppress("LocalVariableName")
        val C = numberOfStates ?: run {
            val task = BasicMin(
                scenarioTree,
                null,
                null,
                null,
                solverProducer
            )
            val automaton = task.infer(isOnlyC = true) ?: return null
            automaton.numberOfStates
        }

        val task = Extended(
            scenarioTree,
            counterExampleTree,
            C,
            maxOutgoingTransitions,
            maxGuardSize,
            solverProducer
        )
        var best: Automaton? = task.infer(initialMaxTotalGuardsSize, finalize = false)

        if (best != null) {
            while (true) {
                @Suppress("LocalVariableName")
                val N = best!!.totalGuardsSize - 1
                println("[*] Trying N = $N...")
                best = task.infer(N, finalize = false) ?: break
            }
        }

        task.finalize()

        return best
    }
}
