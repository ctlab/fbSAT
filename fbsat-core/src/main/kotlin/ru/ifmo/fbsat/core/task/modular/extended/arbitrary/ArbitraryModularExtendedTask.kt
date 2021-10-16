package ru.ifmo.fbsat.core.task.modular.extended.arbitrary

import com.github.lipen.satlib.card.Cardinality
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.constraints.declareArbitraryModularGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareArbitraryModularGuardConditionsConstraints
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.Globals

data class ArbitraryModularExtendedTask(
    val maxGuardSize: Int, // P
    val maxTotalGuardsSize: Int? = null, // N, unconstrained if null
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        comment("$name: Variables")
        declareArbitraryModularExtendedVariables(
            P = maxGuardSize
        )

        /* Constraints */
        comment("$name: Constraints")
        declareArbitraryModularGuardConditionsConstraints()
        if (Globals.IS_BFS_GUARD) declareArbitraryModularGuardConditionsBfsConstraints()

        /* Initial cardinality constraints*/
        comment("$name: Initial cardinality (N) constraints")
        val cardinalityN: Cardinality = context["cardinalityN"]
        cardinalityN.declareUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
}
