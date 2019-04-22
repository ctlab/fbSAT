package ru.ifmo.fbsat.core.task.extended

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.basic.BasicMinTask
import java.io.File

interface ExtendedMinUBTask {
    val scenarioTree: ScenarioTree
    val initialMaxTotalGuardsSize: Int? // N_init, unconstrained if null
    val maxPlateauWidth: Int? // w, unconstrained (=Inf) if null
    val outDir: File

    fun infer(): Automaton?

    companion object {
        @JvmStatic
        fun create(
            scenarioTree: ScenarioTree,
            initialMaxTotalGuardsSize: Int? = null, // N_init, unconstrained if null
            maxPlateauWidth: Int?, // w, unconstrained (=Inf) if null
            outDir: File,
            solverProvider: () -> Solver,
            isEncodeReverseImplication: Boolean = true
        ): ExtendedMinUBTask = ExtendedMinUBTaskImpl(
            scenarioTree = scenarioTree,
            initialMaxTotalGuardsSize = initialMaxTotalGuardsSize,
            maxPlateauWidth = maxPlateauWidth,
            outDir = outDir,
            solverProvider = solverProvider,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
    }
}

private class ExtendedMinUBTaskImpl(
    override val scenarioTree: ScenarioTree,
    // FIXME: seems like N_init support is not needed
    override val initialMaxTotalGuardsSize: Int?, // N_init, unconstrained if null
    override val maxPlateauWidth: Int?, // w, unconstrained (=Inf) if null
    override val outDir: File,
    private val solverProvider: () -> Solver,
    private val isEncodeReverseImplication: Boolean
) : ExtendedMinUBTask {
    @Suppress("LocalVariableName")
    override fun infer(): Automaton? {
        val taskBasicMin = BasicMinTask.create(
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
            println("Trying P = $P")

            if (best != null && P > (best.totalGuardsSize - Tmin)) {
                println("[!] Reached upper bound: P = $P, Nbest = ${best.totalGuardsSize}, Tmin = $Tmin")
                break
            }

            if (Plow != null && maxPlateauWidth != null && (P - Plow) > maxPlateauWidth) {
                println("[!] Reached maximum plateau width: P = $P, Plow = $Plow, w = $maxPlateauWidth")
                break
            }

            val task = ExtendedMinTask.create(
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
