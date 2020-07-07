package ru.ifmo.fbsat.core.task.distributed

import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.distributedBasicVars

fun Inferrer.distributedBasic(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
): DistributedAutomaton? {
    reset()
    solver.declareDistributedBasic(
        scenarioTree = scenarioTree,
        numberOfModules = numberOfModules,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions,
        maxTransitions = maxTransitions,
        isEncodeReverseImplication = isEncodeReverseImplication
    )
    return inferDistributedBasic()
}

fun Inferrer.inferDistributedBasic(): DistributedAutomaton? {
    val rawAssignment = solver.solve() ?: return null
    val vars = solver.distributedBasicVars
    val assignment = DistributedBasicAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    // with(vars) {
    //     check(
    //         automaton.checkMapping(
    //             multiScenarios = listOf(scenarioTree.scenarios, scenarioTree.scenarios),
    //             modularMapping = assignment.modularBasicAssignment.map { it.mapping }
    //         )
    //     ) { "Positive mapping mismatch" }
    // }

    return automaton
}
