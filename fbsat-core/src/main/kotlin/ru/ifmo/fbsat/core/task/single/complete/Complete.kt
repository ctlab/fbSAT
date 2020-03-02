package ru.ifmo.fbsat.core.task.single.complete

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.single.Inferrer
import ru.ifmo.fbsat.core.task.single.basic.basicMinC
import ru.ifmo.fbsat.core.task.single.basic.declareBasic
import ru.ifmo.fbsat.core.task.single.extended.declareExtended
import ru.ifmo.fbsat.core.task.single.extended.inferExtended
import ru.ifmo.fbsat.core.task.single.extended.optimizeN

fun Inferrer.complete(
    scenarioTree: ScenarioTree,
    negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null // N, unconstrained if null
): Automaton? {
    reset()
    solver.declareBasic(
        scenarioTree = scenarioTree,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions,
        maxTransitions = maxTransitions,
        isEncodeReverseImplication = false
    )
    solver.declareExtended(maxGuardSize = maxGuardSize, maxTotalGuardsSize = maxTotalGuardsSize)
    solver.declareComplete(negativeScenarioTree)
    return inferExtended()
}

fun Inferrer.completeMin(
    scenarioTree: ScenarioTree,
    negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
    maxGuardSize: Int // P
): Automaton? {
    val basicAutomaton = basicMinC(scenarioTree)
    // Note: we have to reset because basicMinC uses isEncodeReverseImplication = true,
    //  which is incompatible with negative reduction
    reset()
    solver.declareBasic(
        scenarioTree = scenarioTree,
        numberOfStates = basicAutomaton.numberOfStates,
        isEncodeReverseImplication = false
    )
    solver.declareExtended(maxGuardSize = maxGuardSize)
    solver.declareComplete(negativeScenarioTree)
    return optimizeN()
}

fun Inferrer.completeMinUB(): Automaton? {
    TODO("completeMinUB")
}
