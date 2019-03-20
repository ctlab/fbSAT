package ru.ifmo.fbsat.task.basicmin

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.task.basic.BasicTask

class BasicMinTask(
    val scenarioTree: ScenarioTree,
    val numberOfStates: Int?, // C, search if null
    val maxOutgoingTransitions: Int?, // K, =C if null
    val initialMaxTransitions: Int?, // T_init, unconstrained if null
    val solverProvider: () -> Solver
) {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions != null)) {
            "do not specify only K"
        }
    }

    fun infer(isOnlyC: Boolean = false): Automaton? {
        var best: Automaton? = null
        var task: BasicTask? = null

        if (numberOfStates == null) {
            for (C in 1..20) {
                task = BasicTask(
                    scenarioTree,
                    null,
                    C,
                    maxOutgoingTransitions,
                    solverProvider
                )
                val automaton = task.infer(initialMaxTransitions)
                if (automaton != null) {
                    best = automaton
                    break
                }
            }
        } else {
            task = BasicTask(
                scenarioTree,
                null,
                numberOfStates,
                maxOutgoingTransitions,
                solverProvider
            )
            best = task.infer(initialMaxTransitions)
        }

        if (!isOnlyC && best != null) {
            while (true) {
                @Suppress("LocalVariableName")
                val T = best!!.numberOfTransitions - 1
                println("Trying T = $T...")
                val automaton = task!!.infer(T) ?: break
                best = automaton
            }
        }

        task!!.finalize()

        return best
    }
}
