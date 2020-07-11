package ru.ifmo.fbsat.core.task.single.extforest

import ru.ifmo.fbsat.core.constraints.declareExtForestGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.basicVars
import ru.ifmo.fbsat.core.task.extForestVars

data class ExtForestTask(
    val totalNodes: Int, // P
    val maxTotalGuardsSize: Int? = null // N, unconstrained if null
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        val vars = declareExtForestVariables(
            basicVars = context.basicVars,
            P = totalNodes
        ).also {
            context.extForestVars = it
        }

        /* Constraints */
        declareExtForestGuardConditionsConstraints(vars)

        /* Initial cardinality constraints */
        vars.cardinality.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
}
