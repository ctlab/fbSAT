package ru.ifmo.fbsat.core.task.single.extended

import ru.ifmo.fbsat.core.constraints.declareGuardConditionsAdhocConstraints
import ru.ifmo.fbsat.core.constraints.declareGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.Globals

data class ExtendedTask(
    val maxGuardSize: Int, // P
    val maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    val useAssumptions: Boolean = false,
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        comment("$name: Variables")
        declareExtendedVariables(
            P = maxGuardSize,
            useAssumptions = useAssumptions,
        )

        /* Constraints */
        comment("$name: Constraints")
        declarePositiveGuardConditionsConstraints()
        if (Globals.IS_BFS_GUARD) declareGuardConditionsBfsConstraints()
        declareGuardConditionsAdhocConstraints()

        /* Initial cardinality constraints */
        comment("$name: Initial cardinality (N) constraints")
        val cardinalityN: Cardinality = context["cardinalityN"]
        cardinalityN.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
}
