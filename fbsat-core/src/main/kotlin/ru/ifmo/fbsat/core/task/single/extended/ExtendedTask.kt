package ru.ifmo.fbsat.core.task.single.extended

import ru.ifmo.fbsat.core.constraints.declareGuardConditionsAdhocConstraints
import ru.ifmo.fbsat.core.constraints.declareGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.basicVars
import ru.ifmo.fbsat.core.task.extendedVars
import ru.ifmo.fbsat.core.utils.Globals

data class ExtendedTask(
    val maxGuardSize: Int, // P
    val maxTotalGuardsSize: Int? = null // N, unconstrained if null
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        val vars = declareExtendedVariables(
            basicVars = context.basicVars,
            P = maxGuardSize
        ).also {
            context.extendedVars = it
        }

        /* Constraints */
        declarePositiveGuardConditionsConstraints(vars)
        if (Globals.IS_BFS_GUARD) declareGuardConditionsBfsConstraints(vars)
        declareGuardConditionsAdhocConstraints(vars)

        /* Initial cardinality constraints */
        vars.cardinality.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
}
