package ru.ifmo.fbsat.core.task.single.basic

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.basicVars
import ru.ifmo.fbsat.core.task.optimizeTopDown
import ru.ifmo.fbsat.core.utils.checkMapping
import ru.ifmo.fbsat.core.utils.log

fun Inferrer.inferBasic(): Automaton? {
    val rawAssignment = solver.solve() ?: return null
    val vars = solver.basicVars
    val assignment = BasicAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    with(vars) {
        check(
            automaton.checkMapping(
                scenarios = scenarioTree.scenarios,
                mapping = assignment.mapping
            )
        ) { "Positive mapping mismatch" }
    }

    return automaton
}

fun Inferrer.optimizeT(start: Int? = null, end: Int = 0): Automaton? {
    log.info("Optimizing T...")
    val vars = solver.basicVars
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { T ->
            vars.cardinality.updateUpperBoundLessThanOrEqual(T)
            inferBasic()
        },
        next = { T ->
            vars.cardinality.updateUpperBoundLessThan(T)
            inferBasic()
        },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.basic(
    scenarioTree: ScenarioTree,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
): Automaton? {
    reset()
    solver.declareBasic(
        scenarioTree = scenarioTree,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions,
        maxTransitions = maxTransitions,
        isEncodeReverseImplication = isEncodeReverseImplication
    )
    return inferBasic()
}

fun Inferrer.basicMinC(
    scenarioTree: ScenarioTree,
    start: Int = 1, // C_start
    end: Int = 20 // C_end
): Automaton {
    var best: Automaton? = null
    for (C in start..end) {
        reset()
        solver.declareBasic(scenarioTree, numberOfStates = C)
        val (result, runningTime) = measureTimeWithResult { inferBasic() }
        if (result != null) {
            log.success("BasicMin: C = $C -> SAT in %.2f s.".format(runningTime.seconds))
            log.info("BasicMin: minimal C = $C")
            best = result
            break
        } else {
            log.failure("BasicMin: C = $C -> UNSAT in %.2f s.".format(runningTime.seconds))
        }
    }
    return checkNotNull(best) { "BasicMin: automaton not found." }
}

fun Inferrer.basicMin(scenarioTree: ScenarioTree): Automaton? {
    basicMinC(scenarioTree)
    return optimizeT()
}
