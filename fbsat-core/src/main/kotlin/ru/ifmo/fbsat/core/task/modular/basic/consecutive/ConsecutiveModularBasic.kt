package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.consecutiveModularBasicVars
import ru.ifmo.fbsat.core.task.optimizeTopDown
import ru.ifmo.fbsat.core.utils.log

fun Inferrer.inferConsecutiveModularBasic(): ConsecutiveModularAutomaton? {
    val rawAssignment = solver.solve() ?: return null
    val vars = solver.consecutiveModularBasicVars
    val assignment = ConsecutiveModularBasicAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    // TODO: check automaton
    check(true)

    return automaton
}

fun Inferrer.optimizeConsecutiveModularT(start: Int? = null, end: Int = 0): ConsecutiveModularAutomaton? {
    log.info("Optimizing T...")
    val vars = solver.consecutiveModularBasicVars
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { T ->
            vars.cardinality.updateUpperBoundLessThanOrEqual(T)
            inferConsecutiveModularBasic()
        },
        next = { T ->
            vars.cardinality.updateUpperBoundLessThan(T)
            inferConsecutiveModularBasic()
        },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.consecutiveModularBasic(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
): ConsecutiveModularAutomaton? {
    reset()
    solver.declareConsecutiveModularBasic(
        scenarioTree = scenarioTree,
        numberOfModules = numberOfModules,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions,
        maxTransitions = maxTransitions,
        isEncodeReverseImplication = isEncodeReverseImplication
    )
    return inferConsecutiveModularBasic()
}

fun Inferrer.consecutiveModularBasicMinC(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    start: Int = 1, // C_start
    end: Int = 20 // C_end
): ConsecutiveModularAutomaton {
    var best: ConsecutiveModularAutomaton? = null
    for (C in start..end) {
        reset()
        solver.declareConsecutiveModularBasic(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = C
        )
        val (result, runningTime) = measureTimeWithResult { inferConsecutiveModularBasic() }
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
    scenarioTree: ScenarioTree,
    numberOfModules: Int // M
): ConsecutiveModularAutomaton? {
    consecutiveModularBasicMinC(scenarioTree, numberOfModules = numberOfModules)
    return optimizeConsecutiveModularT()
}
