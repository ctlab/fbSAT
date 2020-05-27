package ru.ifmo.fbsat.core.task.single.extforest

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.constraints.declareExtForestGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.VARS
import ru.ifmo.fbsat.core.task.basicVars
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

fun Solver.declareExtForest(
    totalNodes: Int, // P
    maxTotalGuardsSize: Int? = null // N, unconstrained if null
) {
    val timeStart = PerformanceCounter.reference
    val nvarStart = numberOfVariables
    val nconStart = numberOfClauses

    /* Variables */
    val vars = declareExtForestVariables(
        basicVars = basicVars,
        P = totalNodes
    ).also {
        context[VARS.EXTFOREST] = it
    }

    /* Constraints */
    declareExtForestGuardConditionsConstraints(vars)

    /* Initial cardinality constraints */
    vars.cardinality.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)

    val nvarDiff = numberOfVariables - nvarStart
    val nconDiff = numberOfClauses - nconStart
    log.info(
        "Done declaring extForest variables ($nvarDiff) and constraints ($nconDiff) in %.3f s."
            .format(timeSince(timeStart).seconds)
    )
}
