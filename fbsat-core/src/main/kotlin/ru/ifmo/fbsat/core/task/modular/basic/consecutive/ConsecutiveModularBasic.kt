package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.automaton.buildBasicArbitraryModularAutomaton
import ru.ifmo.fbsat.core.automaton.buildBasicConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeConsecutiveModularT
import ru.ifmo.fbsat.core.utils.log

fun Inferrer.consecutiveModularBasic(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
): ConsecutiveModularAutomaton? {
    reset()
    declare(
        ConsecutiveModularBasicTask(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = maxTransitions,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
    )
    return inferConsecutiveModularBasic()
}

fun Inferrer.consecutiveModularBasicMinC(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    start: Int = 1, // C_start
    end: Int = 20 // C_end
): ConsecutiveModularAutomaton {
    var best: ConsecutiveModularAutomaton? = null
    for (C in start..end) {
        val (result, runningTime) = measureTimeWithResult {
            consecutiveModularBasic(
                scenarioTree = scenarioTree,
                numberOfModules = numberOfModules,
                numberOfStates = C
            )
        }
        if (result != null) {
            log.success("ConsecutiveModularBasicMin: C = $C -> SAT in %.3f s.".format(runningTime.seconds))
            log.info("ConsecutiveModularBasicMin: minimal C = $C")
            best = result
            break
        } else {
            log.failure("ConsecutiveModularBasicMin: C = $C -> UNSAT in %.3f s.".format(runningTime.seconds))
        }
    }
    return checkNotNull(best) { "ConsecutiveModularBasicMin: automaton not found." }
}

fun Inferrer.consecutiveModularBasicMin(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int // M
): ConsecutiveModularAutomaton? {
    consecutiveModularBasicMinC(scenarioTree, numberOfModules = numberOfModules)
    return optimizeConsecutiveModularT()
}

fun Inferrer.inferConsecutiveModularBasic(): ConsecutiveModularAutomaton? {
    val rawAssignment = solver.solve() ?: return null
    val automaton = buildBasicConsecutiveModularAutomaton(
        context = solver.context,
        raw = rawAssignment
    )

    // TODO: check automaton
    // log.warn("Mapping check is not implemented yet")

    return automaton
}
