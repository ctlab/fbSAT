package ru.ifmo.fbsat.core.task.distributed.extended

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.constraints.declareDistributedGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareDistributedPositiveGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.forEachModularContext
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.multiArrayOfNulls

data class DistributedExtendedTask(
    val numberOfModules: Int, // M
    val modularMaxGuardSize: MultiArray<Int>, // [P]
    val modularMaxTotalGuardsSize: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [N]
    val maxTotalGuardsSize: Int? = null, // N_sum, unconstrained if null
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
        forEachModularContext { m ->
            comment("$name: Initial cardinality (T) constraints: for module m = $m")
            val cardinalityN: Cardinality = context["cardinalityN"]
            cardinalityN.updateUpperBoundLessThanOrEqual(modularMaxTotalGuardsSize[m])
        }
        comment("$name: Initial cardinality (T) constraints")
        val cardinalityN: Cardinality = context["cardinalityN"]
        cardinalityN.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
}
