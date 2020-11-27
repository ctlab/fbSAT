package ru.ifmo.fbsat.core.task.modular.basic.parallel

import ru.ifmo.fbsat.core.constraints.declareParallelModularAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareParallelModularAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveParallelModularMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.Globals

data class ParallelModularBasicTask(
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
        declareParallelModularBasicVariables(
            scenarioTree = scenarioTree,
            M = numberOfModules,
            C = numberOfStates,
            K = maxOutgoingTransitions ?: numberOfStates
        )

        /* Constraints */
        comment("$name: Constraints")
        declareParallelModularAutomatonStructureConstraints()
        if (Globals.IS_BFS_AUTOMATON) declareParallelModularAutomatonBfsConstraints()
        declarePositiveParallelModularMappingConstraints(isEncodeReverseImplication)

        /* Initial cardinality constraints */
        comment("$name: Initial cardinality (T) constraints")
        val cardinalityT: Cardinality = context["cardinalityT"]
        cardinalityT.updateUpperBoundLessThanOrEqual(maxTransitions)
    }
}
