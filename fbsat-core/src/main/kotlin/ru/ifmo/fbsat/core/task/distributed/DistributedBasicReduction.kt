package ru.ifmo.fbsat.core.task.distributed

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.constraints.declareDistributedAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareDistributedAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declareDistributedPositiveMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.VARS
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

fun Solver.declareDistributedBasic(
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
    val vars = declareDistributedBasicVariables(
        scenarioTree = scenarioTree,
        M = numberOfModules,
        C = numberOfStates,
        K = maxOutgoingTransitions ?: numberOfStates
    ).also {
        context[VARS.DISTRIBUTED_BASIC] = it
    }

    /* Constraints */
    declareDistributedAutomatonStructureConstraints(vars)
    if (Globals.IS_BFS_AUTOMATON) declareDistributedAutomatonBfsConstraints(vars)
    declareDistributedPositiveMappingConstraints(vars, isEncodeReverseImplication = isEncodeReverseImplication)

    /* Initial cardinality constraints */
    vars.cardinality.updateUpperBoundLessThanOrEqual(maxTransitions)

    val nvarDiff = numberOfVariables - nvarStart
    val nconDiff = numberOfClauses - nconStart
    log.info(
        "Done declaring distributed basic variables ($nvarDiff) and constraints ($nconDiff) in %.3f s."
            .format(timeSince(timeStart).seconds)
    )
}
