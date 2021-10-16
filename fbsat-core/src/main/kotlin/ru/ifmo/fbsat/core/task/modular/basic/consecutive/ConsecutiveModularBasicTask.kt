package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import com.github.lipen.satlib.card.Cardinality
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveConsecutiveModularMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.Globals

data class ConsecutiveModularBasicTask(
    val scenarioTree: PositiveScenarioTree,
    val numberOfModules: Int, // M
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int? = null, // K, =C if null
    val maxTransitions: Int? = null, // T, unconstrained if null
    val isEncodeReverseImplication: Boolean = true,
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        comment("$name: Variables")
        declareConsecutiveModularBasicVariables(
            scenarioTree = scenarioTree,
            M = numberOfModules,
            C = numberOfStates,
            K = maxOutgoingTransitions ?: numberOfStates
        )

        /* Constraints */
        comment("$name: Constraints")
        declareConsecutiveModularAutomatonStructureConstraints()
        if (Globals.IS_BFS_AUTOMATON) declareConsecutiveModularAutomatonBfsConstraints()
        declarePositiveConsecutiveModularMappingConstraints(isEncodeReverseImplication)

        /* Initial cardinality constraints */
        comment("$name: Initial cardinality (T) constraints")
        val cardinalityT: Cardinality = context["cardinalityT"]
        cardinalityT.declareUpperBoundLessThanOrEqual(maxTransitions)
    }
}
