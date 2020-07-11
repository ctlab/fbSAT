package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveConsecutiveModularMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.consecutiveModularBasicVars
import ru.ifmo.fbsat.core.utils.Globals

data class ConsecutiveModularBasicTask(
    val scenarioTree: ScenarioTree,
    val numberOfModules: Int, // M
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int? = null, // K, =C if null
    val maxTransitions: Int? = null, // T, unconstrained if null
    val isEncodeReverseImplication: Boolean = true
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        val vars = declareConsecutiveModularBasicVariables(
            scenarioTree = scenarioTree,
            M = numberOfModules,
            C = numberOfStates,
            K = maxOutgoingTransitions ?: numberOfStates
        ).also {
            context.consecutiveModularBasicVars = it
        }

        /* Constraints */
        declareConsecutiveModularAutomatonStructureConstraints(vars)
        if (Globals.IS_BFS_AUTOMATON) declareConsecutiveModularAutomatonBfsConstraints(vars)
        declarePositiveConsecutiveModularMappingConstraints(vars, isEncodeReverseImplication)

        /* Initial cardinality constraints */
        vars.cardinality.updateUpperBoundLessThanOrEqual(maxTransitions)
    }
}
