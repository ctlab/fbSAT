package ru.ifmo.fbsat.core.task.distributed.extended

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.constraints.declareDistributedGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareDistributedPositiveGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.distributedBasicVars
import ru.ifmo.fbsat.core.task.distributedExtendedVars
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.multiArrayOfNulls

data class DistributedExtendedTask(
    val numberOfModules: Int, // M
    val modularMaxGuardSize: MultiArray<Int>, // [P]
    val modularMaxTotalGuardsSize: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [N]
    val maxTotalGuardsSize: Int? = null // N_sum, unconstrained if null
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        val vars = declareDistributedExtendedVariables(
            basicVars = context.distributedBasicVars,
            modularP = modularMaxGuardSize
        ).also {
            context.distributedExtendedVars = it
        }

        /* Constraints */
        declareDistributedPositiveGuardConditionsConstraints(vars)
        if (Globals.IS_BFS_GUARD) declareDistributedGuardConditionsBfsConstraints(vars)

        /* Initial cardinality constraints */
        for (m in 1..numberOfModules) {
            vars.modularExtendedVariables[m].cardinality.updateUpperBoundLessThanOrEqual(modularMaxTotalGuardsSize[m])
        }
        vars.cardinality.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
}
