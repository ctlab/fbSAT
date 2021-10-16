package ru.ifmo.fbsat.core.task.single.extforest

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeN_Forest
import ru.ifmo.fbsat.core.task.single.basic.BasicTask
import ru.ifmo.fbsat.core.task.single.basic.basicMinC

fun Inferrer.extForest(
    scenarioTree: PositiveScenarioTree,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    totalNodes: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    isEncodeReverseImplication: Boolean = true,
): Automaton? {
    reset()
    declare(
        BasicTask(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = maxTransitions,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
    )
    declare(
        ExtForestTask(
            totalNodes = totalNodes,
            maxTotalGuardsSize = maxTotalGuardsSize
        )
    )
    return inferExtForest()
}

fun Inferrer.extForestMin(
    scenarioTree: PositiveScenarioTree,
    totalNodes: Int, // P
): Automaton? {
    return run {
        basicMinC(scenarioTree) ?: return@run null
        declare(ExtForestTask(totalNodes = totalNodes))
        optimizeN_Forest()
    }
}

fun Inferrer.inferExtForest(): Automaton? {
    val model = solveAndGetModel() ?: return null
    val automaton = TODO()

    // TODO: check mapping
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
