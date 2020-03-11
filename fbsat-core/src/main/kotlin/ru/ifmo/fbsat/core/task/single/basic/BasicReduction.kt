package ru.ifmo.fbsat.core.task.single.basic

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.constraints.declareAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.BASIC_VARS
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

fun Solver.declareBasic(
    scenarioTree: ScenarioTree,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
) {
    val timeStart = PerformanceCounter.reference
    val nvarStart = numberOfVariables
    val nconStart = numberOfClauses

    /* Variables */
    val vars = declareBasicVariables(
        scenarioTree = scenarioTree,
        C = numberOfStates,
        K = maxOutgoingTransitions ?: numberOfStates
    ).also {
        context[BASIC_VARS] = it
    }

    /* Constraints */
    declareAutomatonStructureConstraints(vars)
    if (Globals.IS_BFS_AUTOMATON) declareAutomatonBfsConstraints(vars)
    declarePositiveMappingConstraints(vars, isEncodeReverseImplication = isEncodeReverseImplication)

    /* Initial cardinality constraints */
    vars.cardinality.updateUpperBoundLessThanOrEqual(maxTransitions)

    val nvarDiff = numberOfVariables - nvarStart
    val nconDiff = numberOfClauses - nconStart
    log.info(
        "Done declaring Basic variables ($nvarDiff) and constraints ($nconDiff) in %.3f s."
            .format(timeSince(timeStart).seconds)
    )
}
