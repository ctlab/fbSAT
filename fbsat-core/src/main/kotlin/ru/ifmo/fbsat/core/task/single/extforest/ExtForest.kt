package ru.ifmo.fbsat.core.task.single.extforest

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.extForestVars
import ru.ifmo.fbsat.core.task.optimizeTopDown
import ru.ifmo.fbsat.core.task.single.basic.basicMinC
import ru.ifmo.fbsat.core.task.single.basic.declareBasic
import ru.ifmo.fbsat.core.utils.log

fun Inferrer.inferExtForest(): Automaton? {
    val vars = solver.extForestVars
    val rawAssignment = solver.solve() ?: return null
    val assignment = ExtForestAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    // with(vars) {
    //     check(
    //         automaton.checkMapping(
    //             scenarios = scenarioTree.scenarios,
    //             mapping = assignment.mapping
    //         )
    //     ) { "Positive mapping mismatch" }
    // }

    return automaton
}

fun Inferrer.optimizeN_Forest(start: Int? = null, end: Int = 0): Automaton? {
    log.info("Optimizing N...")
    val vars = solver.extForestVars
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { T ->
            vars.cardinality.updateUpperBoundLessThanOrEqual(T)
            inferExtForest()
        },
        next = { T ->
            vars.cardinality.updateUpperBoundLessThan(T)
            inferExtForest()
        },
        query = { it.totalGuardsSize }
    )
}

fun Inferrer.extForest(
    scenarioTree: ScenarioTree,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    totalNodes: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
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
    solver.declareExtForest(
        totalNodes = totalNodes,
        maxTotalGuardsSize = maxTotalGuardsSize
    )
    return inferExtForest()
}

fun Inferrer.extForestMin(
    scenarioTree: ScenarioTree,
    totalNodes: Int // P
): Automaton? {
    basicMinC(scenarioTree)
    solver.declareExtForest(totalNodes = totalNodes)
    return optimizeN_Forest()
}
