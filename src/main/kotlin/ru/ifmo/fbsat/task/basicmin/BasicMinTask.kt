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
    val solverProvider: () -> Solver,
    val isEncodeTransitionsOrder: Boolean
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
                println("Trying C = $C")
                task = BasicTask(
                    scenarioTree = scenarioTree,
                    numberOfStates = C,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    solverProvider = solverProvider,
                    isEncodeTransitionsOrder = isEncodeTransitionsOrder
                )
                val automaton = task.infer(initialMaxTransitions, finalize = false)
                if (automaton != null) {
                    best = automaton
                    break
                } else {
                    task.finalize()
                }
            }
        } else {
            task = BasicTask(
                scenarioTree = scenarioTree,
                numberOfStates = numberOfStates,
                maxOutgoingTransitions = maxOutgoingTransitions,
                solverProvider = solverProvider,
                isEncodeTransitionsOrder = isEncodeTransitionsOrder
            )
            best = task.infer(initialMaxTransitions, finalize = false)
        }

        if (!isOnlyC && best != null) {
            while (true) {
                @Suppress("LocalVariableName")
                val T = best!!.numberOfTransitions - 1
                println("Trying T = $T...")
                val automaton = task!!.infer(T, finalize = false) ?: break
                best = automaton
            }
        }

        task!!.finalize()

        return best
    }
}
