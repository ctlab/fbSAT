package ru.ifmo.fbsat.core.task.modular.extended.parallel

import ru.ifmo.fbsat.core.automaton.ParallelModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.modular.basic.parallel.parallelModularBasic
import ru.ifmo.fbsat.core.task.modular.basic.parallel.parallelModularBasicMinC
import ru.ifmo.fbsat.core.task.optimizeTopDown
import ru.ifmo.fbsat.core.task.parallelModularExtendedVars
import ru.ifmo.fbsat.core.utils.log


fun Inferrer.inferParallelModularExtended(): ParallelModularAutomaton? {
    val rawAssignment = solver.solve() ?: return null
    val vars = solver.parallelModularExtendedVars
    val assignment = ParallelModularExtendedAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    // TODO: check automaton
    check(true)

    return automaton
}

fun Inferrer.optimizeParallelModularN(start: Int? = null, end: Int = 0): ParallelModularAutomaton? {
    log.info("Optimizing N...")
    val vars = solver.parallelModularExtendedVars
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { N ->
            vars.cardinality.updateUpperBoundLessThanOrEqual(N)
            inferParallelModularExtended()
        },
        next = { N ->
            vars.cardinality.updateUpperBoundLessThan(N)
            inferParallelModularExtended()
        },
        query = { it.totalGuardsSize }
    )
}

fun Inferrer.parallelModularExtended(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    isEncodeReverseImplication: Boolean = true
): ParallelModularAutomaton? {
    reset()
    parallelModularBasic(
        scenarioTree = scenarioTree,
        numberOfModules = numberOfModules,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions,
        maxTransitions = maxTransitions,
        isEncodeReverseImplication = isEncodeReverseImplication
    )
    solver.declareParallelModularExtended(
        maxGuardSize = maxGuardSize,
        maxTotalGuardsSize = maxTotalGuardsSize
    )
    return inferParallelModularExtended()
}

fun Inferrer.parallelModularExtendedMin(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int? = null, // C_start, 1 if null
    maxGuardSize: Int // P
): ParallelModularAutomaton? {
    parallelModularBasicMinC(scenarioTree, numberOfModules, start = numberOfStates ?: 1)
    solver.declareParallelModularExtended(maxGuardSize)
    return optimizeParallelModularN()
}
