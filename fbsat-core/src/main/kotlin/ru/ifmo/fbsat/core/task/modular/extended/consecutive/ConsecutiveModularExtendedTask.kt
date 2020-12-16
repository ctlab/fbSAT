package ru.ifmo.fbsat.core.task.modular.extended.consecutive

import com.github.lipen.satlib.card.Cardinality
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularGuardConditionsBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularGuardConditionsConstraints
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.Globals

data class ConsecutiveModularExtendedTask(
    val maxGuardSize: Int, // P
    val maxTotalGuardsSize: Int? = null, // N, unconstrained if null
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        comment("$name: Variables")
        declareConsecutiveModularExtendedVariables(
            P = maxGuardSize
        )

        /* Constraints */
        comment("$name: Constraints")
        declareConsecutiveModularGuardConditionsConstraints()
        if (Globals.IS_BFS_GUARD) declareConsecutiveModularGuardConditionsBfsConstraints()

        /* Initial cardinality constraints*/
        // ======================
        // forEachModularContext { m ->
        //     comment("$name: Initial cardinality (N) constraints: for module m = $m")
        //     val cardinalityN: Cardinality = context["cardinalityN"]
        //     cardinalityN.declareUpperBoundLessThanOrEqual(...)
        // }
        // ======================
        comment("$name: Initial cardinality (N) constraints")
        val cardinalityN: Cardinality = context["cardinalityN"]
        cardinalityN.declareUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
}
