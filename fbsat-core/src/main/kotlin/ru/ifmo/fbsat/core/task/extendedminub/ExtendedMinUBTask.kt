package ru.ifmo.fbsat.core.task.extendedminub

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.basicmin.BasicMinTask
import ru.ifmo.fbsat.core.task.extendedmin.ExtendedMinTask
import java.io.File

class ExtendedMinUBTask(
    val scenarioTree: ScenarioTree,
    val negativeScenarioTree: NegativeScenarioTree?,
    // FIXME: seems like N_init support is not needed
    val initialMaxTotalGuardsSize: Int?, // N_init, unconstrained if null
    val maxPlateauWidth: Int?, // w, unconstrained (=Inf) if null
    val outDir: File,
    private val solverProvider: () -> Solver,
    private val isForbidLoops: Boolean = true,
    private val isEncodeAutomaton: Boolean = false,
    private val isEncodeTransitionsOrder: Boolean,
    private val isEncodeReverseImplication: Boolean
) {
    @Suppress("LocalVariableName")
    fun infer(): Automaton? {
        val taskBasicMin = BasicMinTask(
            scenarioTree = scenarioTree,
            numberOfStates = null,
            maxOutgoingTransitions = null,
            initialMaxTransitions = null,
            outDir = outDir,
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
                outDir = outDir,
                solverProvider = solverProvider,
                isForbidLoops = isForbidLoops,
                isEncodeAutomaton = isEncodeAutomaton,
                isEncodeTransitionsOrder = isEncodeTransitionsOrder,
                isEncodeReverseImplication = isEncodeReverseImplication
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
