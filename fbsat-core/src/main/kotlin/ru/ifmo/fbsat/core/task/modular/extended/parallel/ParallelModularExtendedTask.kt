package ru.ifmo.fbsat.core.task.modular.extended.parallel

import ru.ifmo.fbsat.core.constraints.declareParallelModularGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareParallelModularGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.parallelModularBasicVars
import ru.ifmo.fbsat.core.task.parallelModularExtendedVars
import ru.ifmo.fbsat.core.utils.Globals

data class ParallelModularExtendedTask(
    val maxGuardSize: Int, // P
    val maxTotalGuardsSize: Int? = null // N, unconstrained if null
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        val vars = declareParallelModularExtendedVariables(
            basicVars = context.parallelModularBasicVars,
            P = maxGuardSize
        ).also {
            context.parallelModularExtendedVars = it
        }

        /* Constraints */
        declareParallelModularGuardConditionsConstraints(vars)
        if (Globals.IS_BFS_GUARD) declareParallelModularGuardConditionsBfsConstraints(vars)

        /* Initial cardinality constraints*/
        vars.cardinality.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
}
