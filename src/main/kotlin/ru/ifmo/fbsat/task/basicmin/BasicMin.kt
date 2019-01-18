package ru.ifmo.fbsat.task.basicmin

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.task.basic.Basic

class BasicMin(
    val scenarioTree: ScenarioTree,
    val numberOfStates: Int?, // C, search if null
    val maxOutgoingTransitions: Int?, // K, =C if null
    val initialMaxTransitions: Int?, // T_init, unconstrained if null
    val solverProducer: () -> Solver
) {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions == null)) {
            "do not specify only K"
        }
    }

    fun infer(isOnlyC: Boolean = false): Automaton? {
        var best: Automaton? = null
        var task: Basic? = null

        if (numberOfStates == null) {
            for (C in 1..20) {
                task = Basic(
                    scenarioTree,
                    null,
                    C,
                    maxOutgoingTransitions,
                    solverProducer
                )
                val automaton = task.infer(initialMaxTransitions)
                if (automaton != null) {
                    best = automaton
                    break
                }
            }
        } else {
            task = Basic(
                scenarioTree,
                null,
                numberOfStates,
                maxOutgoingTransitions,
                solverProducer
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
