package ru.ifmo.fbsat.core.task.single.complete

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeN
import ru.ifmo.fbsat.core.task.single.basic.BasicTask
import ru.ifmo.fbsat.core.task.single.basic.basicMinC
import ru.ifmo.fbsat.core.task.single.extended.ExtendedTask
import ru.ifmo.fbsat.core.task.single.extended.inferExtended

fun Inferrer.complete(
    scenarioTree: PositiveScenarioTree,
    negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null // N, unconstrained if null
): Automaton? {
    reset()
    declare(
        BasicTask(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = maxTransitions,
            isEncodeReverseImplication = false
        )
    )
    declare(ExtendedTask(maxGuardSize = maxGuardSize, maxTotalGuardsSize = maxTotalGuardsSize))
    declare(CompleteTask(negativeScenarioTree))
    return inferExtended()
}

fun Inferrer.completeMin(
    scenarioTree: PositiveScenarioTree,
    negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
    maxGuardSize: Int // P
): Automaton? {
    val basicAutomaton = basicMinC(scenarioTree)
    // Note: we have to reset because basicMinC uses isEncodeReverseImplication = true,
    //  which is incompatible with negative reduction
    // TODO: check whether isEncodeReverseImplication is actually true in current basicVars
    reset()
    declare(
        BasicTask(
            scenarioTree = scenarioTree,
            numberOfStates = basicAutomaton.numberOfStates,
            isEncodeReverseImplication = false
        )
    )
    declare(ExtendedTask(maxGuardSize = maxGuardSize))
    declare(CompleteTask(negativeScenarioTree))
    return optimizeN()
}

fun Inferrer.completeMinUB(): Automaton? {
    TODO("completeMinUB")
}
