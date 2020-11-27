package ru.ifmo.fbsat.core.task.single.extforest

import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.constraints.declareExtForestGuardConditionsConstraints
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.task.Task

data class ExtForestTask(
    val totalNodes: Int, // P
    val maxTotalGuardsSize: Int? = null, // N, unconstrained if null
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        comment("$name: Variables")
        declareExtForestVariables(
            P = totalNodes
        )

        /* Constraints */
        comment("$name: Constraints")
        declareExtForestGuardConditionsConstraints()

        /* Initial cardinality constraints */
        comment("$name: Initial cardinality (N) constraints")
        val cardinalityN: Cardinality = context["cardinalityN"]
        cardinalityN.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
}
