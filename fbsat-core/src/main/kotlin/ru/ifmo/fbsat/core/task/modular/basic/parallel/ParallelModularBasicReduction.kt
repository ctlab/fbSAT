package ru.ifmo.fbsat.core.task.modular.basic.parallel

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.constraints.declareParallelModularAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareParallelModularAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveParallelModularMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.VARS
import ru.ifmo.fbsat.core.task.parallelModularBasicVars
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

fun Solver.declareParallelModularBasic(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
) {
    val timeStart = PerformanceCounter.reference
    val nvarStart = numberOfVariables
    val nconStart = numberOfClauses

    /* Variables */
    val vars = declareParallelModularBasicVariables(
        scenarioTree = scenarioTree,
        M = numberOfModules,
        C = numberOfStates,
        K = maxOutgoingTransitions ?: numberOfStates
    ).also {
        context[VARS.PARALLEL_MODULAR_BASIC] = it
    }

    /* Constraints */
    declareParallelModularAutomatonStructureConstraints(vars)
    if (Globals.IS_BFS_AUTOMATON) declareParallelModularAutomatonBfsConstraints(vars)
    declarePositiveParallelModularMappingConstraints(vars, isEncodeReverseImplication)
    declareParallelModularBasicAdhocConstraints()

    /* Initial cardinality constraints */
    vars.cardinality.updateUpperBoundLessThanOrEqual(maxTransitions)

    val nvarDiff = numberOfVariables - nvarStart
    val nconDiff = numberOfClauses - nconStart
    log.info(
        "Done declaring parallel modular basic variables ($nvarDiff) and constraints ($nconDiff) in %.3f s."
            .format(timeSince(timeStart).seconds)
    )
}

private fun Solver.declareParallelModularBasicAdhocConstraints() {
    val vars = parallelModularBasicVars
    // comment("ADHOC constraints")
}
