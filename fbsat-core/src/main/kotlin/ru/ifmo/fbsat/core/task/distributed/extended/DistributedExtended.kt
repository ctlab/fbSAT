package ru.ifmo.fbsat.core.task.distributed.extended

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.distributed.basic.DistributedBasicTask
import ru.ifmo.fbsat.core.task.distributedExtendedVars
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.magic
import ru.ifmo.fbsat.core.utils.multiArrayOfNulls

fun Inferrer.distributedExtended(
    numberOfModules: Int, // M
    modularScenarioTree: MultiArray<PositiveScenarioTree>,
    modularNumberOfStates: MultiArray<Int>, // [C]
    modularMaxOutgoingTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [K]
    modularMaxGuardSize: MultiArray<Int>, // [P]
    modularMaxTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [T]
    modularMaxTotalGuardsSize: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [N]
    modularIsEncodeReverseImplication: MultiArray<Boolean> = MultiArray.create(numberOfModules) { true },
    maxTransitions: Int? = null, // T_sum, unconstrained if null
    maxTotalGuardsSize: Int? = null // N_sum, unconstrained if null
): DistributedAutomaton? {
    reset()
    declare(
        DistributedBasicTask(
            numberOfModules = numberOfModules,
            compoundScenarioTree = magic(),
            modularScenarioTree = modularScenarioTree,
            modularNumberOfStates = modularNumberOfStates,
            modularMaxOutgoingTransitions = modularMaxOutgoingTransitions,
            modularMaxTransitions = modularMaxTransitions,
            modularIsEncodeReverseImplication = modularIsEncodeReverseImplication,
            maxTransitions = maxTransitions
        )
    )
    declare(
        DistributedExtendedTask(
            numberOfModules = numberOfModules,
            modularMaxGuardSize = modularMaxGuardSize,
            modularMaxTotalGuardsSize = modularMaxTotalGuardsSize,
            maxTotalGuardsSize = maxTotalGuardsSize
        )
    )
    return inferDistributedExtended()
}

fun Inferrer.inferDistributedExtended(): DistributedAutomaton? {
    val rawAssignment = solver.solve() ?: return null
    val vars = solver.context.distributedExtendedVars
    val assignment = DistributedExtendedAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    // TODO: check mapping
    log.warn("Mapping check is not implemented yet")

    return automaton
}
