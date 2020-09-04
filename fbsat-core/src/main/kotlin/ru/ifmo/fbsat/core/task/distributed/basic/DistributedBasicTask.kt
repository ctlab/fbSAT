package ru.ifmo.fbsat.core.task.distributed.basic

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.mapIndexed
import ru.ifmo.fbsat.core.constraints.declareDistributedAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareDistributedAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declareDistributedPositiveMappingConstraints_compound
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.distributedBasicVars
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.multiArrayOfNulls

data class DistributedBasicTask(
    val numberOfModules: Int, // M
    val compoundScenarioTree: PositiveCompoundScenarioTree,
    val modularScenarioTree: MultiArray<PositiveScenarioTree>,
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
            compoundScenarioTree = compoundScenarioTree,
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
        // declareDistributedPositiveMappingConstraints_modular(vars, modularIsEncodeReverseImplication)
        declareDistributedPositiveMappingConstraints_compound(vars, modularIsEncodeReverseImplication)
        // declareDistributedBasicAdhocConstraints(vars)

        /* Initial cardinality constraints */
        for (m in 1..numberOfModules) {
            vars.modularBasicVariables[m].cardinality.updateUpperBoundLessThanOrEqual(modularMaxTransitions[m])
        }
        vars.cardinality.updateUpperBoundLessThanOrEqual(maxTransitions)
    }
}

private fun Solver.declareDistributedBasicAdhocConstraints(vars: DistributedBasicVariables){
    with(vars.modularBasicVariables[1]) {
        check(C == 4)
        check(K > 2)
        clause(transitionDestination[1, 1] eq 2)
        clause(transitionDestination[1, 2] eq 0)

        clause(transitionDestination[2, 1] eq 3)
        clause(transitionDestination[2, 2] eq 2)
        clause(transitionDestination[2, 3] eq 0)

        clause(transitionDestination[3, 1] eq 4)
        clause(transitionDestination[3, 2] eq 0)

        clause(transitionDestination[4, 1] eq 1)
        clause(transitionDestination[4, 2] eq 4)
        clause(transitionDestination[4, 3] eq 0)
    }
}
