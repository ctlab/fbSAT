package ru.ifmo.fbsat.core.task.single.basic2

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.ECC
import ru.ifmo.fbsat.core.scenario2.positive.ScenarioTree2
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.basicVars2
import ru.ifmo.fbsat.core.task.optimizeTopDown
import ru.ifmo.fbsat.core.utils.log

fun Inferrer.inferBasic2(): ECC? {
    val rawAssignment = solver.solve() ?: return null
    val vars = solver.basicVars2
    val assignment = BasicAssignment2.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    return automaton
}

fun Inferrer.optimizeT2(start: Int? = null, end: Int = 0): ECC? {
    log.info("Optimizing T...")
    val vars = solver.basicVars2
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { T ->
            vars.cardinality.updateUpperBoundLessThanOrEqual(T)
            inferBasic2()
        },
        next = { T ->
            vars.cardinality.updateUpperBoundLessThan(T)
            inferBasic2()
        },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.basic2(
    scenarioTree: ScenarioTree2,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
): ECC? {
    reset()
    solver.declareBasic2(
        scenarioTree = scenarioTree,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions,
        maxTransitions = maxTransitions,
        isEncodeReverseImplication = isEncodeReverseImplication
    )
    return inferBasic2()
}

fun Inferrer.basicMinC2(
    scenarioTree: ScenarioTree2,
    start: Int = 1, // C_start
    end: Int = 20, // C_end
    isEncodeReverseImplication: Boolean = true
): ECC {
    var best: ECC? = null
    for (C in start..end) {
        reset()
        solver.declareBasic2(scenarioTree, numberOfStates = C, isEncodeReverseImplication = isEncodeReverseImplication)
        val (result, runningTime) = measureTimeWithResult { inferBasic2() }
        if (result != null) {
            log.success("BasicMin: C = $C -> SAT in %.3f s.".format(runningTime.seconds))
            log.info("BasicMin: minimal C = $C")
            best = result
            break
        } else {
            log.failure("BasicMin: C = $C -> UNSAT in %.3f s.".format(runningTime.seconds))
        }
    }
    return checkNotNull(best) { "BasicMin: automaton not found." }
}

fun Inferrer.basicMin2(
    scenarioTree: ScenarioTree2,
    isEncodeReverseImplication: Boolean = true
): ECC? {
    basicMinC2(scenarioTree, isEncodeReverseImplication = isEncodeReverseImplication)
    return optimizeT2()
}
