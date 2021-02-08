package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.automaton.buildBasicConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.solveAndGetModel
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeConsecutiveModularT
import ru.ifmo.fbsat.core.utils.MyLogger

private val logger = MyLogger {}

fun Inferrer.consecutiveModularBasic(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true,
): ConsecutiveModularAutomaton? {
    reset()
    declare(
        ConsecutiveModularBasicTask(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = maxTransitions,
            isEncodeReverseImplication = isEncodeReverseImplication,
        )
    )
    return inferConsecutiveModularBasic()
}

fun Inferrer.consecutiveModularBasicMinC(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    start: Int = 1, // C_start
    end: Int = 20, // C_end
): ConsecutiveModularAutomaton {
    var best: ConsecutiveModularAutomaton? = null
    for (C in start..end) {
        val (result, runningTime) = measureTimeWithResult {
            consecutiveModularBasic(
                scenarioTree = scenarioTree,
                numberOfModules = numberOfModules,
                numberOfStates = C,
            )
        }
        if (result != null) {
            logger.info("ConsecutiveModularBasicMin: C = $C -> SAT in %.3f s.".format(runningTime.seconds))
            logger.info("ConsecutiveModularBasicMin: minimal C = $C")
            best = result
            break
        } else {
            logger.info("ConsecutiveModularBasicMin: C = $C -> UNSAT in %.3f s.".format(runningTime.seconds))
        }
    }
    return checkNotNull(best) { "ConsecutiveModularBasicMin: automaton not found." }
}

fun Inferrer.consecutiveModularBasicMin(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int? = null, // C_start, 1 if null
): ConsecutiveModularAutomaton? {
    consecutiveModularBasicMinC(scenarioTree, numberOfModules = numberOfModules, start = numberOfStates ?: 1)
    return optimizeConsecutiveModularT()
}

fun Inferrer.inferConsecutiveModularBasic(): ConsecutiveModularAutomaton? {
    val model = solver.solveAndGetModel() ?: return null
    val automaton = buildBasicConsecutiveModularAutomaton(solver.context, model)

    // TODO: check automaton
    // log.warn("Mapping check is not implemented yet")

    return automaton
}
