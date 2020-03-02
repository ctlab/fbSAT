package ru.ifmo.fbsat.core.task.modular.extended.consecutive

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.single.CONSECUTIVE_MODULAR_EXTENDED_VARS
import ru.ifmo.fbsat.core.task.single.consecutiveModularBasicVars
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

fun Solver.declareConsecutiveModularExtended(
    maxGuardSize: Int, // P
    maxTotalGuardsSize: Int? = null // N, unconstrained if null
) {
    val timeStart = PerformanceCounter.reference
    val nvarStart = numberOfVariables
    val nconStart = numberOfClauses

    /* Variables */
    val vars = declareConsecutiveModularExtendedVariables(
        basicVars = consecutiveModularBasicVars,
        P = maxGuardSize
    ).also {
        context[CONSECUTIVE_MODULAR_EXTENDED_VARS] = it
    }

    /* Constraints */
    declareConsecutiveModularGuardConditionsConstraints(vars)
    if (Globals.IS_BFS_GUARD) declareConsecutiveModularGuardConditionsBfsConstraints(vars)
    declareConsecutiveModularExtendedAdhocConstraints()

    /* Initial cardinality constraints*/
    vars.cardinality.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)

    val nvarDiff = numberOfVariables - nvarStart
    val nconDiff = numberOfClauses - nconStart
    log.info(
        "Done declaring consecutive modular extended variables ($nvarDiff) and constraints ($nconDiff) in %.2f s."
            .format(timeSince(timeStart).seconds)
    )
}

fun Solver.declareConsecutiveModularExtendedAdhocConstraints() {
    val vars = consecutiveModularBasicVars
    // comment("ADHOC constraints")
}
