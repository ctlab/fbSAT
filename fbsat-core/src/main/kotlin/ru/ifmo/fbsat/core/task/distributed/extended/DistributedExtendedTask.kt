package ru.ifmo.fbsat.core.task.distributed.extended

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.constraints.declareDistributedGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareDistributedPositiveGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.SolverContext
import ru.ifmo.fbsat.core.solver.switchContext
import ru.ifmo.fbsat.core.task.Task
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
        comment("$name: Variables")
        declareDistributedExtendedVariables(
            modularP = modularMaxGuardSize
        )

        /* Constraints */
        comment("$name: Constraints")
        declareDistributedPositiveGuardConditionsConstraints()
        if (Globals.IS_BFS_GUARD) declareDistributedGuardConditionsBfsConstraints()

        /* Initial cardinality constraints */
        val modularContext: MultiArray<SolverContext> by context
        val M: Int by context
        for (m in 1..M)  switchContext(modularContext[m]){
            comment("$name: Initial cardinality (T) constraints: for module m = $m")
            val cardinalityN: Cardinality by context
            cardinalityN.updateUpperBoundLessThanOrEqual(modularMaxTotalGuardsSize[m])
        }
        comment("$name: Initial cardinality (T) constraints")
        val cardinalityN: Cardinality by context
        cardinalityN.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
}
