package ru.ifmo.fbsat.core.task.complete

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.extended.ExtendedMinTask
import ru.ifmo.fbsat.core.utils.log
import java.io.File

class CompleteMinCegisTask(
    private val scenarioTree: ScenarioTree,
    private val initialNegativeScenarioTree: NegativeScenarioTree? = null,
    private val numberOfStates: Int?, // C, search if null
    private val maxOutgoingTransitions: Int?, // K, K=C if null
    private val maxGuardSize: Int, // P
    private val initialMaxTotalGuardsSize: Int?, // N_init, unconstrained if null
    val smvDir: File,
    val outDir: File,
    val solverProvider: () -> Solver
) {
    init {
        require(!(numberOfStates == null && maxOutgoingTransitions != null)) { "do not specify only K" }
    }

    @Suppress("LocalVariableName")
    fun infer(): Automaton? {
        val extendedMinTask = ExtendedMinTask(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxGuardSize = maxGuardSize,
            initialMaxTotalGuardsSize = initialMaxTotalGuardsSize,
            outDir = outDir,
            solverProvider = solverProvider,
            isEncodeReverseImplication = true // Note: we encode reverse implication to quickly estimate an automaton
        )
        val automatonExtendedMin = extendedMinTask.infer()
            ?: error("ExtendedMinTask could not infer an automaton")
        val C = automatonExtendedMin.numberOfStates
        // val K = maxOutgoingTransitions
        // Note: reusing K from extMinTask may fail!
        val K = automatonExtendedMin.maxOutgoingTransitions
        log.info("Reusing K = $K")
        val P = maxGuardSize
        var N = automatonExtendedMin.totalGuardsSize

        log.info("automatonExtendedMin:")
        automatonExtendedMin.pprint()
        log.info("automatonExtendedMin has C = $C, P = $P, N = $N")

        val negativeScenarioTree = initialNegativeScenarioTree ?: NegativeScenarioTree(
            inputEvents = scenarioTree.inputEvents,
            outputEvents = scenarioTree.outputEvents,
            inputNames = scenarioTree.inputNames,
            outputNames = scenarioTree.outputNames
        )

        for (loopNumber in 1..100) {
            log.just("===== Loop number #$loopNumber =====")

            val task = CompleteCegisTask(
                scenarioTree = scenarioTree,
                negativeScenarioTree = negativeScenarioTree,
                numberOfStates = C,
                maxOutgoingTransitions = K,
                maxGuardSize = P,
                maxTotalGuardsSize = N,
                smvDir = smvDir,
                outDir = outDir,
                solver = solverProvider()
            )

            val automaton = task.infer()
            if (automaton != null) {
                log.just("Hooray! Minimal full verified automaton has been found!")
                return automaton
            } else {
                log.just("UNSAT, N = $N is too small, trying larger value...")
                N += 1
                if (N > C * K * P) {
                    log.error("N reached upper bound C*K*P = ${C * K * P}")
                    break
                }
            }
        }

        return null
    }
}
