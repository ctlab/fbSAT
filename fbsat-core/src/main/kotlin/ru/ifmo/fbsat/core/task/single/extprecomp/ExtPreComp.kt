package ru.ifmo.fbsat.core.task.single.extprecomp

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.solveAndGetModel
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeN
import ru.ifmo.fbsat.core.task.single.basic.BasicTask
import ru.ifmo.fbsat.core.task.single.basic.basicMinC
import ru.ifmo.fbsat.core.utils.MyLogger

private val logger = MyLogger {}

fun Inferrer.extPreComp(
    scenarioTree: PositiveScenarioTree,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
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
        ExtPreCompTask(
            maxGuardSize = maxGuardSize,
            maxTotalGuardsSize = maxTotalGuardsSize
        )
    )
    return inferExtPreComp()
}

fun Inferrer.extPreCompMin(
    scenarioTree: PositiveScenarioTree,
    numberOfStates: Int? = null, // C_start
    maxGuardSize: Int, // P
): Automaton? {
    basicMinC(scenarioTree, start = numberOfStates ?: 1) // ?: return null
    declare(ExtPreCompTask(maxGuardSize = maxGuardSize))
    return optimizeN()
}

fun Inferrer.inferExtPreComp(): Automaton? {
    val model = solver.solveAndGetModel() ?: return null
    val automaton = TODO()

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
