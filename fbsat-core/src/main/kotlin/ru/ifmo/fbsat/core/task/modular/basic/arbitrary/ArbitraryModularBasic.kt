package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.ArbitraryModularAutomaton
import ru.ifmo.fbsat.core.automaton.buildBasicArbitraryModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeArbitraryModularT
import ru.ifmo.fbsat.core.utils.MyLogger

private val logger = MyLogger {}

fun Inferrer.arbitraryModularBasic(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true,
): ArbitraryModularAutomaton? {
    reset()
    declare(
        ArbitraryModularBasicTask(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = maxTransitions,
            isEncodeReverseImplication = isEncodeReverseImplication,
        )
    )
    solver.dumpDimacs(outDir.resolve("cnf_modular-arbitrary-basic_M${numberOfModules}_C${numberOfStates}.cnf"))
    return inferArbitraryModularBasic()
}

fun Inferrer.arbitraryModularBasicMinC(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    start: Int = 1, // C_start
    end: Int = 20, // C_end
): ArbitraryModularAutomaton {
    var best: ArbitraryModularAutomaton? = null
    for (C in start..end) {
        val (result, runningTime) = measureTimeWithResult {
            arbitraryModularBasic(
                scenarioTree = scenarioTree,
                numberOfModules = numberOfModules,
                numberOfStates = C,
            )
        }
        if (result != null) {
            logger.info("ArbitraryModularBasicMin: C = $C -> SAT in %.3f s.".format(runningTime.seconds))
            logger.info("ArbitraryModularBasicMin: minimal C = $C")
            best = result
            break
        } else {
            logger.info("ArbitraryModularBasicMin: C = $C -> UNSAT in %.3f s.".format(runningTime.seconds))
        }
    }
    return checkNotNull(best) { "ArbitraryModularBasicMin: automaton not found." }
}

fun Inferrer.arbitraryModularBasicMin(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int? = null, // C_start, 1 if null
): ArbitraryModularAutomaton? {
    arbitraryModularBasicMinC(scenarioTree, numberOfModules = numberOfModules, start = numberOfStates ?: 1)
    return optimizeArbitraryModularT()
}

fun Inferrer.inferArbitraryModularBasic(): ArbitraryModularAutomaton? {
    val model = solveAndGetModel() ?: return null
    val automaton = buildBasicArbitraryModularAutomaton(solver.context, model)

    // TODO: check automaton
    // log.warn("Mapping check is not implemented yet")

    return automaton
}
