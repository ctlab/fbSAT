package ru.ifmo.fbsat.core.task.basic.single

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.core.solver.declareTotalizer
import ru.ifmo.fbsat.core.utils.Globals

@Suppress("MemberVisibilityCanBePrivate")
class BasicVariables(
    val scenarioTree: ScenarioTree,
    // Constants
    val C: Int,
    val K: Int,
    val V: Int,
    val E: Int,
    val O: Int,
    val X: Int,
    val Z: Int,
    val U: Int,
    // Variables
    val transition: IntMultiArray,
    val actualTransition: IntMultiArray,
    val inputEvent: IntMultiArray,
    val outputEvent: IntMultiArray,
    val algorithm0: IntMultiArray,
    val algorithm1: IntMultiArray,
    val color: IntMultiArray,
    val rootValue: IntMultiArray,
    val firstFired: IntMultiArray,
    val notFired: IntMultiArray
) {
    // Cardinality
    var totalizer: IntArray? = null
        private set
    var maxTransitions: Int? = null
        private set

    fun Solver.updateCardinality(newMaxTransitions: Int?) {
        maxTransitions?.let { T ->
            check(newMaxTransitions != null && newMaxTransitions <= T) { "Cannot soften UB" }
        }

        if (newMaxTransitions == null && !Globals.IS_ENCODE_TOTALIZER) return
        if (totalizer == null) {
            totalizer = declareTotalizer {
                for (c in 1..C)
                    for (k in 1..K)
                        yield(-transition[c, k, C + 1])
            }
        }
        if (newMaxTransitions == null) return

        declareComparatorLessThanOrEqual(totalizer!!, newMaxTransitions, maxTransitions)
        maxTransitions = newMaxTransitions
    }

    companion object {
        // fun Solver.declare(scenarioTree: ScenarioTree, C: Int, K: Int): BasicVariables {
        //     // Constants
        //     val V = scenarioTree.size
        //     val E = scenarioTree.inputEvents.size
        //     val O = scenarioTree.outputEvents.size
        //     val X = scenarioTree.inputNames.size
        //     val Z = scenarioTree.outputNames.size
        //     val U = scenarioTree.uniqueInputs.size
        //
        //     // Variables
        //     val transition = newArray(C, K, C + 1)
        //     val actualTransition = newArray(C, E, U, C + 1)
        //     val inputEvent = newArray(C, K, E + 1)
        //     val outputEvent = newArray(C, O + 1)
        //     val algorithm0 = newArray(C, Z)
        //     val algorithm1 = newArray(C, Z)
        //     val color = newArray(V, C)
        //     val rootValue = newArray(C, K, U)
        //     val firstFired = newArray(C, U, K + 1)
        //     val notFired = newArray(C, U, K)
        //
        //     return BasicVariables(
        //         scenarioTree = scenarioTree,
        //         C = C, K = K, V = V, E = E, O = O, X = X, Z = Z, U = U,
        //         transition = transition,
        //         actualTransition = actualTransition,
        //         inputEvent = inputEvent,
        //         outputEvent = outputEvent,
        //         algorithm0 = algorithm0,
        //         algorithm1 = algorithm1,
        //         color = color,
        //         rootValue = rootValue,
        //         firstFired = firstFired,
        //         notFired = notFired
        //     )
        // }
    }
}
