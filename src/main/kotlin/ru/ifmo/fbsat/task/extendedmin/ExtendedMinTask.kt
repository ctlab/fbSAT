package ru.ifmo.fbsat.task.extendedmin

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.task.basicmin.BasicMinTask
import ru.ifmo.fbsat.task.extended.ExtendedTask

class ExtendedMinTask(
    val scenarioTree: ScenarioTree,
    val negativeScenarioTree: NegativeScenarioTree?,
    val numberOfStates: Int?, // C, search if null
    val maxOutgoingTransitions: Int?, // K, =C if null
    val maxGuardSize: Int, // P
    val initialMaxTotalGuardsSize: Int?, // N_init, unconstrained if null
    val solverProvider: () -> Solver,
    private val isForbidLoops: Boolean = true,
    private val isEncodeAutomaton: Boolean = false,
    private val isEncodeTransitionsOrder: Boolean
) {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions != null)) {
            "do not specify only K"
        }
    }

    @Suppress("LocalVariableName")
    fun infer(): Automaton? {
        val C = numberOfStates ?: run {
            val task = BasicMinTask(
                scenarioTree,
                null,
                null,
                null,
                solverProvider
            )
            val automaton = task.infer(isOnlyC = true) ?: return null
            automaton.numberOfStates
        }

        val task = ExtendedTask(
            scenarioTree,
            negativeScenarioTree,
            C,
            maxOutgoingTransitions,
            maxGuardSize,
            solverProvider,
            isForbidLoops = isForbidLoops,
            isEncodeAutomaton = isEncodeAutomaton,
            isEncodeTransitionsOrder = isEncodeTransitionsOrder
        )
        var best: Automaton? = task.infer(initialMaxTotalGuardsSize, finalize = false)

        if (best != null) {
            while (true) {
                val N = best!!.totalGuardsSize - 1
                // // ==============
                // if (N < 23) break
                // // ==============
                println("[*] Trying N = $N...")
                best = task.infer(N, finalize = false) ?: break
            }
        }

        task.finalize()

        return best
    }
}
