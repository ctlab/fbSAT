package ru.ifmo.fbsat.core.task.modular.extended.consecutive

import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.consecutiveModularBasicVars
import ru.ifmo.fbsat.core.task.consecutiveModularExtendedVars
import ru.ifmo.fbsat.core.utils.Globals

data class ConsecutiveModularExtendedTask(
    val maxGuardSize: Int, // P
    val maxTotalGuardsSize: Int? = null // N, unconstrained if null
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        val vars = declareConsecutiveModularExtendedVariables(
            basicVars = context.consecutiveModularBasicVars,
            P = maxGuardSize
        ).also {
            context.consecutiveModularExtendedVars = it
        }

        /* Constraints */
        declareConsecutiveModularGuardConditionsConstraints(vars)
        if (Globals.IS_BFS_GUARD) declareConsecutiveModularGuardConditionsBfsConstraints(vars)

        /* Initial cardinality constraints*/
        vars.cardinality.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
}
