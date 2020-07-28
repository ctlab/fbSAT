package ru.ifmo.fbsat.core.task.modular.extended.parallel

import ru.ifmo.fbsat.core.automaton.ParallelModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.modular.basic.parallel.ParallelModularBasicTask
import ru.ifmo.fbsat.core.task.modular.basic.parallel.parallelModularBasicMinC
import ru.ifmo.fbsat.core.task.optimizeParallelModularN
import ru.ifmo.fbsat.core.task.parallelModularExtendedVars
import ru.ifmo.fbsat.core.utils.log

fun Inferrer.parallelModularExtended(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    isEncodeReverseImplication: Boolean = true
): ParallelModularAutomaton? {
    reset()
    declare(
        ParallelModularBasicTask(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = maxTransitions,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
    )
    declare(
        ParallelModularExtendedTask(
            maxGuardSize = maxGuardSize,
            maxTotalGuardsSize = maxTotalGuardsSize
        )
    )
    return inferParallelModularExtended()
}

fun Inferrer.parallelModularExtendedMin(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int? = null, // C_start, 1 if null
    maxGuardSize: Int // P
): ParallelModularAutomaton? {
    parallelModularBasicMinC(scenarioTree, numberOfModules, start = numberOfStates ?: 1)
    declare(ParallelModularExtendedTask(maxGuardSize))
    return optimizeParallelModularN()
}

fun Inferrer.inferParallelModularExtended(): ParallelModularAutomaton? {
    val rawAssignment = solver.solve() ?: return null
    val vars = solver.context.parallelModularExtendedVars
    val assignment = ParallelModularExtendedAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    // TODO: check automaton
    log.warn("Mapping check is not implemented yet")

    return automaton
}
