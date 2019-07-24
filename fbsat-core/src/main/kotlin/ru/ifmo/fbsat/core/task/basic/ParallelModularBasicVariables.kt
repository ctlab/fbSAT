package ru.ifmo.fbsat.core.task.basic

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver

class ParallelModularBasicVariables private constructor(
    val scenarioTree: ScenarioTree,
    // Constants
    val M: Int,
    val C: Int,
    val K: Int,
    val V: Int,
    val E: Int,
    val O: Int,
    val X: Int,
    val Z: Int,
    val U: Int,
    // Variables
    val transitionModular: MultiArray<IntMultiArray>,
    val actualTransitionModular: MultiArray<IntMultiArray>,
    val inputEventModular: MultiArray<IntMultiArray>,
    val outputEventModular: MultiArray<IntMultiArray>,
    val outputVariableModule: IntMultiArray,
    val algorithm0Modular: MultiArray<IntMultiArray>,
    val algorithm1Modular: MultiArray<IntMultiArray>,
    val colorModular: MultiArray<IntMultiArray>,
    val rootValueModular: MultiArray<IntMultiArray>,
    val firstFiredModular: MultiArray<IntMultiArray>,
    val notFiredModular: MultiArray<IntMultiArray>
) {
    // Cardinality
    var totalizer: IntArray? = null
        private set
    var maxTransitions: Int? = null
        private set

    fun Solver.updateCardinality(newMaxTransitions: Int?) {
        totalizer
        maxTransitions
        TODO()
    }

    companion object {
        fun Solver.declare(scenarioTree: ScenarioTree, M: Int, C: Int, K: Int): ParallelModularBasicVariables {
            // Constants
            val V = scenarioTree.size
            val E = scenarioTree.inputEvents.size
            val O = scenarioTree.outputEvents.size
            val X = scenarioTree.inputNames.size
            val Z = scenarioTree.outputNames.size
            val U = scenarioTree.uniqueInputs.size

            // Variables
            val transitionModular = MultiArray.create(M) { newArray(C, K, C + 1) }
            val actualTransitionModular = MultiArray.create(M) { newArray(C, E, U, C + 1) }
            val inputEventModular = MultiArray.create(M) { newArray(C, K, E + 1) }
            val outputEventModular = MultiArray.create(M) { newArray(C, O + 1) }
            val outputVariableModule = newArray(Z, M)
            val algorithm0Modular = MultiArray.create(M) { newArray(C, Z) }
            val algorithm1Modular = MultiArray.create(M) { newArray(C, Z) }
            val colorModular = MultiArray.create(M) { newArray(V, C) }
            val rootValueModular = MultiArray.create(M) { newArray(C, K, U) }
            val firstFiredModular = MultiArray.create(M) { newArray(C, U, K + 1) }
            val notFiredModular = MultiArray.create(M) { newArray(C, U, K) }

            return ParallelModularBasicVariables(
                scenarioTree = scenarioTree,
                M = M, C = C, K = K, V = V, E = E, O = O, X = X, Z = Z, U = U,
                transitionModular = transitionModular,
                actualTransitionModular = actualTransitionModular,
                inputEventModular = inputEventModular,
                outputEventModular = outputEventModular,
                outputVariableModule = outputVariableModule,
                algorithm0Modular = algorithm0Modular,
                algorithm1Modular = algorithm1Modular,
                colorModular = colorModular,
                rootValueModular = rootValueModular,
                firstFiredModular = firstFiredModular,
                notFiredModular = notFiredModular
            )
        }
    }
}
