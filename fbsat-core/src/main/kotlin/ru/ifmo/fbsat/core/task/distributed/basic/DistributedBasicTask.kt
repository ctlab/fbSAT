package ru.ifmo.fbsat.core.task.distributed.basic

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.mapIndexed
import ru.ifmo.fbsat.core.constraints.declareDistributedAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareDistributedAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declareDistributedPositiveMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.distributedBasicVars
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.multiArrayOfNulls

data class DistributedBasicTask(
    val numberOfModules: Int, // M
    val modularScenarioTree: MultiArray<OldPositiveScenarioTree>,
    val modularNumberOfStates: MultiArray<Int>, // [C]
    val modularMaxOutgoingTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [K]
    val modularMaxTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [T]
    val modularIsEncodeReverseImplication: MultiArray<Boolean> = MultiArray.create(numberOfModules) { true },
    val maxTransitions: Int? = null // T, unconstrained if null
) : Task() {
    init {
        require(modularScenarioTree.shape.single() == numberOfModules)
        require(modularNumberOfStates.shape.single() == numberOfModules)
        require(modularMaxOutgoingTransitions.shape.single() == numberOfModules)
        require(modularMaxTransitions.shape.single() == numberOfModules)
        require(modularIsEncodeReverseImplication.shape.single() == numberOfModules)
    }

    override fun Solver.declare_() {
        /* Variables */
        val vars = declareDistributedBasicVariables(
            M = numberOfModules,
            modularScenarioTree = modularScenarioTree,
            modularC = modularNumberOfStates,
            modularK = modularMaxOutgoingTransitions.mapIndexed { (m), k ->
                k ?: modularNumberOfStates[m]
            }
        ).also {
            context.distributedBasicVars = it
        }

        /* Constraints */
        declareDistributedAutomatonStructureConstraints(vars)
        if (Globals.IS_BFS_AUTOMATON) declareDistributedAutomatonBfsConstraints(vars)
        declareDistributedPositiveMappingConstraints(vars, modularIsEncodeReverseImplication)

        /* Initial cardinality constraints */
        for (m in 1..numberOfModules) {
            vars.modularBasicVariables[m].cardinality.updateUpperBoundLessThanOrEqual(modularMaxTransitions[m])
        }
        vars.cardinality.updateUpperBoundLessThanOrEqual(maxTransitions)
    }
}
