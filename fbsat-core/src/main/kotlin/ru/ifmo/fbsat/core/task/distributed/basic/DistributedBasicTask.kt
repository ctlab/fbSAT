package ru.ifmo.fbsat.core.task.distributed.basic

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.mapIndexed
import ru.ifmo.fbsat.core.constraints.declareDistributedAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareDistributedAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declareDistributedPositiveMappingConstraints_compound
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.SolverContext
import ru.ifmo.fbsat.core.solver.switchContext
import ru.ifmo.fbsat.core.task.Task
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
        comment("$name: Variables")
        declareDistributedBasicVariables(
            M = numberOfModules,
            compoundScenarioTree = compoundScenarioTree,
            modularC = modularNumberOfStates,
            modularK = modularMaxOutgoingTransitions.mapIndexed { (m), k ->
                k ?: modularNumberOfStates[m]
            }
        )

        /* Constraints */
        comment("$name: Constraints")
        declareDistributedAutomatonStructureConstraints()
        if (Globals.IS_BFS_AUTOMATON) declareDistributedAutomatonBfsConstraints()
        // declareDistributedPositiveMappingConstraints_modular(vars, modularIsEncodeReverseImplication)
        declareDistributedPositiveMappingConstraints_compound(modularIsEncodeReverseImplication)
        // declareDistributedBasicAdhocConstraints()

        /* Initial cardinality constraints */
        val modularContext: MultiArray<SolverContext> by context
        val M: Int by context
        for (m in 1..M) switchContext(modularContext[m]) {
            comment("$name: Initial cardinality (T) constraints: for module m = $m")
            val cardinalityT: Cardinality by context
            cardinalityT.updateUpperBoundLessThanOrEqual(modularMaxTransitions[m])
        }
        comment("$name: Initial cardinality (T) constraints")
        val cardinalityT: Cardinality by context
        cardinalityT.updateUpperBoundLessThanOrEqual(maxTransitions)
    }
}

private fun Solver.declareDistributedBasicAdhocConstraints() {
    //
}
