package ru.ifmo.fbsat.core.task.modular.extended.parallel

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.constraints.declareParallelModularGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareParallelModularGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.VARS
import ru.ifmo.fbsat.core.task.parallelModularBasicVars
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

fun Solver.declareParallelModularExtended(
    maxGuardSize: Int, // P
    maxTotalGuardsSize: Int? = null // N, unconstrained if null
) {
    val timeStart = PerformanceCounter.reference
    val nvarStart = numberOfVariables
    val nconStart = numberOfClauses

    /* Variables */
    val vars = declareParallelModularExtendedVariables(
        basicVars = parallelModularBasicVars,
        P = maxGuardSize
    ).also {
        context[VARS.PARALLEL_MODULAR_EXTENDED] = it
    }

    /* Constraints */
    declareParallelModularGuardConditionsConstraints(vars)
    if (Globals.IS_BFS_GUARD) declareParallelModularGuardConditionsBfsConstraints(vars)
    declareParallelModularExtendedAdhocConstraints()

    /* Initial cardinality constraints*/
    vars.cardinality.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)

    val nvarDiff = numberOfVariables - nvarStart
    val nconDiff = numberOfClauses - nconStart
    log.info(
        "Done declaring parallel modular extended variables ($nvarDiff) and constraints ($nconDiff) in %.3f s."
            .format(timeSince(timeStart).seconds)
    )
}

fun Solver.declareParallelModularExtendedAdhocConstraints() {
    val vars = parallelModularBasicVars
    // comment("ADHOC constraints")
}
