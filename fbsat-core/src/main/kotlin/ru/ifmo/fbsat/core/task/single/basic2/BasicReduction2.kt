package ru.ifmo.fbsat.core.task.single.basic2

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.constraints2.declareAutomatonBfsConstraints2
import ru.ifmo.fbsat.core.constraints2.declareAutomatonStructureConstraints2
import ru.ifmo.fbsat.core.constraints2.declarePositiveMappingConstraints2
import ru.ifmo.fbsat.core.scenario2.positive.ScenarioTree2
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.BASIC_VARS
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

fun Solver.declareBasic2(
    scenarioTree: ScenarioTree2,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
) {
    val timeStart = PerformanceCounter.reference
    val nvarStart = numberOfVariables
    val nconStart = numberOfClauses

    /* Variables */
    val vars = declareBasicVariables2(
        scenarioTree = scenarioTree,
        C = numberOfStates,
        K = maxOutgoingTransitions ?: numberOfStates
    ).also {
        context[BASIC_VARS] = it
    }

    /* Constraints */
    declareAutomatonStructureConstraints2(vars)
    if (Globals.IS_BFS_AUTOMATON) declareAutomatonBfsConstraints2(vars)
    declarePositiveMappingConstraints2(vars, isEncodeReverseImplication = isEncodeReverseImplication)

    /* Initial cardinality constraints */
    vars.cardinality.updateUpperBoundLessThanOrEqual(maxTransitions)

    val nvarDiff = numberOfVariables - nvarStart
    val nconDiff = numberOfClauses - nconStart
    log.info(
        "Done declaring Basic variables ($nvarDiff) and constraints ($nconDiff) in %.3f s."
            .format(timeSince(timeStart).seconds)
    )
}
