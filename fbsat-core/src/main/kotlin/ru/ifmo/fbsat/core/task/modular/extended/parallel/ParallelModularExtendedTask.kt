package ru.ifmo.fbsat.core.task.modular.extended.parallel

import ru.ifmo.fbsat.core.constraints.declareParallelModularGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareParallelModularGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.Globals

data class ParallelModularExtendedTask(
    val maxGuardSize: Int, // P
    val maxTotalGuardsSize: Int? = null, // N, unconstrained if null
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        comment("$name: Variables")
        declareParallelModularExtendedVariables(
            P = maxGuardSize
        )

        /* Constraints */
        comment("$name: Constraints")
        declareParallelModularGuardConditionsConstraints()
        if (Globals.IS_BFS_GUARD) declareParallelModularGuardConditionsBfsConstraints()

        /* Initial cardinality constraints*/
        comment("$name: Initial cardinality (N) constraints")
        val cardinalityN: Cardinality = context["cardinalityN"]
        cardinalityN.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
}
