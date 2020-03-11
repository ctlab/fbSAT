package ru.ifmo.fbsat.core.task.single.extended

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.constraints.declareGuardConditionsAdhocConstraints
import ru.ifmo.fbsat.core.constraints.declareGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.EXTENDED_VARS
import ru.ifmo.fbsat.core.task.basicVars
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

fun Solver.declareExtended(
    maxGuardSize: Int, // P
    maxTotalGuardsSize: Int? = null // N, unconstrained if null
) {
    val timeStart = PerformanceCounter.reference
    val nvarStart = numberOfVariables
    val nconStart = numberOfClauses

    /* Variables */
    val vars = declareExtendedVariables(
        basicVars = basicVars,
        P = maxGuardSize
    ).also {
        context[EXTENDED_VARS] = it
    }

    /* Constraints */
    declarePositiveGuardConditionsConstraints(vars)
    if (ru.ifmo.fbsat.core.utils.Globals.IS_BFS_GUARD) declareGuardConditionsBfsConstraints(vars)
    declareGuardConditionsAdhocConstraints(vars)

    /* Initial cardinality constraints */
    vars.cardinality.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)

    val nvarDiff = numberOfVariables - nvarStart
    val nconDiff = numberOfClauses - nconStart
    log.info(
        "Done declaring extended variables ($nvarDiff) and constraints ($nconDiff) in %.3f s."
            .format(timeSince(timeStart).seconds)
    )
}
