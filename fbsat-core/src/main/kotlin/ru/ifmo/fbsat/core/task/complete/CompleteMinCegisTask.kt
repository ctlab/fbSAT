package ru.ifmo.fbsat.core.task.complete

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.extended.ExtendedMinTask
import ru.ifmo.fbsat.core.utils.log
import java.io.File

interface CompleteMinCegisTask {
    val scenarioTree: ScenarioTree
    val negativeScenarioTree: NegativeScenarioTree
    val numberOfStates: Int? // C, search if null
    val maxOutgoingTransitions: Int? // K, =C if null
    val maxGuardSize: Int // P
    val initialMaxTotalGuardsSize: Int? // N_init, unconstrained if null
    val smvDir: File
    val outDir: File

    fun infer(): Automaton?

    companion object {
        @JvmStatic
        fun create(
            scenarioTree: ScenarioTree,
            initialNegativeScenarioTree: NegativeScenarioTree? = null, // empty if null
            numberOfStates: Int?, // C, search if null
            maxOutgoingTransitions: Int? = null, // K, K=C if null
            maxGuardSize: Int, // P
            initialMaxTotalGuardsSize: Int? = null, // N_init, unconstrained if null
            smvDir: File,
            outDir: File,
            solverProvider: () -> Solver
        ): CompleteMinCegisTask = CompleteMinCegisTaskImpl(
            scenarioTree = scenarioTree,
            negativeScenarioTree = initialNegativeScenarioTree
                ?: NegativeScenarioTree.empty(scenarioTree.inputNames, scenarioTree.outputNames),
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxGuardSize = maxGuardSize,
            initialMaxTotalGuardsSize = initialMaxTotalGuardsSize,
            smvDir = smvDir,
            outDir = outDir,
            solverProvider = solverProvider
        )
    }
}

private class CompleteMinCegisTaskImpl(
    override val scenarioTree: ScenarioTree,
    override val negativeScenarioTree: NegativeScenarioTree,
    override val numberOfStates: Int?, // C, search if null
    override val maxOutgoingTransitions: Int?, // K, K=C if null
    override val maxGuardSize: Int, // P
    override val initialMaxTotalGuardsSize: Int?, // N_init, unconstrained if null
    override val smvDir: File,
    override val outDir: File,
    private val solverProvider: () -> Solver
) : CompleteMinCegisTask {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions != null)) {
            "do not specify only K"
        }
    }

    @Suppress("LocalVariableName")
    override fun infer(): Automaton? {
        val extendedMinTask = ExtendedMinTask.create(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxGuardSize = maxGuardSize,
            initialMaxTotalGuardsSize = initialMaxTotalGuardsSize,
            outDir = outDir,
            solverProvider = solverProvider,
            isEncodeReverseImplication = true // Note: we encode reverse implication to quickly estimate automaton
        )
        val automatonExtendedMin = extendedMinTask.infer()
            ?: error("ExtendedMinTask could not infer an automaton")
        val C = automatonExtendedMin.numberOfStates
        val K = maxOutgoingTransitions
        val P = maxGuardSize
        // var N = automatonExtendedMin.totalGuardsSize
        var N = automatonExtendedMin.getN()

        log.info("automatonExtendedMin:")
        automatonExtendedMin.pprint()
        log.info("automatonExtendedMin has C = $C, P = $P, N = $N")

        out@ for (loopNumber in 1..100) {
            log.just("===== Loop number #$loopNumber =====")

            val task = CompleteCegisTask.create(
                scenarioTree = scenarioTree,
                negativeScenarioTree = negativeScenarioTree,
                numberOfStates = C,
                maxOutgoingTransitions = K,
                maxGuardSize = P,
                maxTotalGuardsSize = N,
                smvDir = smvDir,
                outDir = outDir,
                solverProvider = solverProvider
            )

            val automaton = task.infer()
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
