package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import ru.ifmo.fbsat.core.constraints.declareArbitraryModularAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareArbitraryModularAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveArbitraryModularMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.Globals

data class ArbitraryModularBasicTask(
    val positiveScenarioTree: PositiveScenarioTree,
    val numberOfModules: Int, // M
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int? = null, // K, =C if null
    val maxTransitions: Int? = null, // T, unconstrained if null
    val isEncodeReverseImplication: Boolean = true,
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        comment("$name: Variables")
        declareArbitraryModularBasicVariables(
            scenarioTree = positiveScenarioTree,
            M = numberOfModules,
            C = numberOfStates,
            K = maxOutgoingTransitions ?: numberOfStates
        )

        /* Constraints */
        comment("$name: Constraints")
        declareArbitraryModularAutomatonStructureConstraints()
        if (Globals.IS_BFS_AUTOMATON) declareArbitraryModularAutomatonBfsConstraints()
        declarePositiveArbitraryModularMappingConstraints(isEncodeReverseImplication)

        /* Initial cardinality constraints */
        comment("$name: Initial cardinality (T) constraints")
        val cardinalityT: Cardinality = context["cardinalityT"]
        cardinalityT.updateUpperBoundLessThanOrEqual(maxTransitions)
    }
}
