package ru.ifmo.fbsat.core.task.distributed.basic

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.distributedBasicVars
import ru.ifmo.fbsat.core.utils.createNullable
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.multiArrayOfNulls

fun Inferrer.distributedBasic(
    numberOfModules: Int, // M
    modularScenarioTree: MultiArray<OldPositiveScenarioTree>,
    modularNumberOfStates: MultiArray<Int>, // [C]
    modularMaxOutgoingTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [K]
    modularMaxTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [T]
    modularIsEncodeReverseImplication: MultiArray<Boolean> = MultiArray.createNullable(numberOfModules) { true },
    maxTransitions: Int? = null // T_sum, unconstrained if null
): DistributedAutomaton? {
    reset()
    declare(
        DistributedBasicTask(
            numberOfModules = numberOfModules,
            modularScenarioTree = modularScenarioTree,
            modularNumberOfStates = modularNumberOfStates,
            modularMaxOutgoingTransitions = modularMaxOutgoingTransitions,
            modularMaxTransitions = modularMaxTransitions,
            modularIsEncodeReverseImplication = modularIsEncodeReverseImplication,
            maxTransitions = maxTransitions
        )
    )
    return inferDistributedBasic()
}

fun Inferrer.inferDistributedBasic(): DistributedAutomaton? {
    val rawAssignment = solver.solve() ?: return null
    val vars = solver.context.distributedBasicVars
    val assignment = DistributedBasicAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    // TODO: check mapping
    log.warn("Mapping check is not implemented yet")

    return automaton
}
