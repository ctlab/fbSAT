package ru.ifmo.fbsat.task.extendedminub

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.task.basicmin.BasicMinTask
import ru.ifmo.fbsat.task.extendedmin.ExtendedMinTask

class ExtendedMinUBTask(
    val scenarioTree: ScenarioTree,
    val negativeScenarioTree: NegativeScenarioTree?,
    // FIXME: seems like N_init support is not needed
    val initialMaxTotalGuardsSize: Int?, // N_init, unconstrained if null
    val maxPlateauWidth: Int?, // w, unconstrained (=Inf) if null
    val solverProvider: () -> Solver,
    val isForbidLoops: Boolean = true,
    val isEncodeAutomaton: Boolean = false,
    val isEncodeTransitionsOrder: Boolean
) {
    @Suppress("LocalVariableName")
    fun infer(): Automaton? {
        val taskBasicMin = BasicMinTask(
            scenarioTree = scenarioTree,
            numberOfStates = null,
            maxOutgoingTransitions = null,
            initialMaxTransitions = null,
            solverProvider = solverProvider
        )
        val automatonBasicMin = taskBasicMin.infer()
        checkNotNull(automatonBasicMin)

        val C = automatonBasicMin.numberOfStates
        val Tmin = automatonBasicMin.numberOfTransitions
        var best: Automaton? = null
        var Plow: Int? = null
        var N: Int? = initialMaxTotalGuardsSize

        for (P in (1..100)) {
            println("Trying P = $P")

            if (best != null && P > (best.totalGuardsSize - Tmin)) {
                println("[!] Reached upper bound: P = $P, Nbest = ${best.totalGuardsSize}, Tmin = $Tmin")
                break
            }

            if (Plow != null && maxPlateauWidth != null && (P - Plow) > maxPlateauWidth) {
                println("[!] Reached maximum plateau width: P = $P, Plow = $Plow, w = $maxPlateauWidth")
                break
            }

            val task = ExtendedMinTask(
                scenarioTree = scenarioTree,
                negativeScenarioTree = negativeScenarioTree,
                numberOfStates = C,
                maxOutgoingTransitions = null,
                maxGuardSize = P,
                initialMaxTotalGuardsSize = N,
                solverProvider = solverProvider,
                isForbidLoops = isForbidLoops,
                isEncodeAutomaton = isEncodeAutomaton,
                isEncodeTransitionsOrder = isEncodeTransitionsOrder
            )
            val automaton = task.infer()
            if (automaton != null) {
                best = automaton
                Plow = P
                N = best.totalGuardsSize - 1
            }
        }

        return best
    }
}
