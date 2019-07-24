package ru.ifmo.fbsat.core.task.extended.single

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.basic.single.BasicMinTask
import ru.ifmo.fbsat.core.utils.log
import java.io.File

class ExtendedMinUBTask(
    val scenarioTree: ScenarioTree,
    val initialMaxTotalGuardsSize: Int? = null, // N_init, unconstrained if null
    val maxPlateauWidth: Int?, // w, unconstrained (=Inf) if null
    val outDir: File,
    val solverProvider: () -> Solver,
    val isEncodeReverseImplication: Boolean = true
) {
    @Suppress("LocalVariableName")
    fun infer(): Automaton? {
        val taskBasicMin = BasicMinTask(
            scenarioTree = scenarioTree,
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
            log.info("Trying P = $P")

            if (best != null && P > (best.totalGuardsSize - Tmin)) {
                log.warn("Reached upper bound: P = $P, Plow = $Plow, Nbest = ${best.totalGuardsSize}, Tmin = $Tmin")
                break
            }

            if (Plow != null && maxPlateauWidth != null && (P - Plow) > maxPlateauWidth) {
                log.warn("Reached maximum plateau width: P = $P, Plow = $Plow, w = $maxPlateauWidth")
                break
            }

            val task = ExtendedMinTask(
                scenarioTree = scenarioTree,
                numberOfStates = C,
                maxOutgoingTransitions = null,
                maxGuardSize = P,
                initialMaxTotalGuardsSize = N,
                outDir = outDir,
                solverProvider = solverProvider,
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
