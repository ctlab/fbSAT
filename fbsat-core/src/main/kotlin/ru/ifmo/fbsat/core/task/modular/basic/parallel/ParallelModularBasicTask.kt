package ru.ifmo.fbsat.core.task.modular.basic.parallel

import ru.ifmo.fbsat.core.constraints.declareParallelModularAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareParallelModularAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveParallelModularMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.parallelModularBasicVars
import ru.ifmo.fbsat.core.utils.Globals

data class ParallelModularBasicTask(
    val scenarioTree: ScenarioTree,
    val numberOfModules: Int, // M
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int? = null, // K, =C if null
    val maxTransitions: Int? = null, // T, unconstrained if null
    val isEncodeReverseImplication: Boolean = true
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        val vars = declareParallelModularBasicVariables(
            scenarioTree = scenarioTree,
            M = numberOfModules,
            C = numberOfStates,
            K = maxOutgoingTransitions ?: numberOfStates
        ).also {
            context.parallelModularBasicVars = it
        }

        /* Constraints */
        declareParallelModularAutomatonStructureConstraints(vars)
        if (Globals.IS_BFS_AUTOMATON) declareParallelModularAutomatonBfsConstraints(vars)
        declarePositiveParallelModularMappingConstraints(vars, isEncodeReverseImplication)

        /* Initial cardinality constraints */
        vars.cardinality.updateUpperBoundLessThanOrEqual(maxTransitions)
    }
}
