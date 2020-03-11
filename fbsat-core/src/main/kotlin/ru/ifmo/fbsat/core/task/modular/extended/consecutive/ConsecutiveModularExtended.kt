package ru.ifmo.fbsat.core.task.modular.extended.consecutive

import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.consecutiveModularExtendedVars
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.declareConsecutiveModularBasic

fun Inferrer.inferConsecutiveModularExtended(): ConsecutiveModularAutomaton? {
    val rawAssignment = solver.solve() ?: return null
    val vars = solver.consecutiveModularExtendedVars
    val assignment = ConsecutiveModularExtendedAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    // TODO: check automaton
    check(true)

    return automaton
}

fun Inferrer.consecutiveModularExtended(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
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
    solver.declareConsecutiveModularExtended(
        maxGuardSize = maxGuardSize,
        maxTotalGuardsSize = maxTotalGuardsSize
    )
    return inferConsecutiveModularExtended()
}
