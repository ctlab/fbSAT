package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import ru.ifmo.fbsat.core.constraints.declareArbitraryModularAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareArbitraryModularAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveArbitraryModularMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.arbitraryModularBasicVars
import ru.ifmo.fbsat.core.utils.Globals

data class ArbitraryModularBasicTask(
    val scenarioTree: PositiveScenarioTree,
    val numberOfModules: Int, // M
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int? = null, // K, =C if null
    val maxTransitions: Int? = null, // T, unconstrained if null
    val isEncodeReverseImplication: Boolean = true
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        val vars = declareArbitraryModularBasicVariables(
            scenarioTree = scenarioTree,
            M = numberOfModules,
            C = numberOfStates,
            K = maxOutgoingTransitions ?: numberOfStates
        ).also {
            context.arbitraryModularBasicVars = it
        }

        /* Constraints */
        declareArbitraryModularAutomatonStructureConstraints(vars)
        if (Globals.IS_BFS_AUTOMATON) declareArbitraryModularAutomatonBfsConstraints(vars)
        declarePositiveArbitraryModularMappingConstraints(vars, isEncodeReverseImplication)

        /* Initial cardinality constraints */
        vars.cardinality.updateUpperBoundLessThanOrEqual(maxTransitions)
    }
}
