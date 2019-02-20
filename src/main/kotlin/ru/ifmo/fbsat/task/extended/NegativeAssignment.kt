package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.scenario.NegativeScenarioTree
import ru.ifmo.fbsat.utils.BooleanMultiArray
import ru.ifmo.fbsat.utils.IntMultiArray

class NegativeAssignment(
    val negativeScenarioTree: NegativeScenarioTree,
    val C: Int,
    val K: Int,
    val P: Int,
    val satisfaction: IntMultiArray,
    val actualTransition: IntMultiArray,
    val nodeValue: BooleanMultiArray,
    val firstFired: BooleanMultiArray,
    val notFired: BooleanMultiArray
) {
    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(
            raw: BooleanArray,
            reduction: NegativeReduction
        ): NegativeAssignment {
            val negTree = reduction.negativeScenarioTree
            // Constants
            val C = reduction.C
            val K = reduction.K
            val P = reduction.P
            val V = negTree.size
            val E = negTree.inputEvents.size
            val U = negTree.uniqueInputs.size
            // Counterexample variables
            val satisfaction = IntMultiArray.new(V) { (v) ->
                (1..C).firstOrNull { c -> raw[reduction.satisfaction[v, c] - 1] }
                    ?: 0
            }
            // Automaton variables
            val actualTransition = IntMultiArray.new(C, E, U) { (i, e, u) ->
                (1..C).firstOrNull { j -> raw[reduction.actualTransition[i, e, u, j] - 1] }
                    ?: 0
            }
            // Guards variables
            val nodeValue = BooleanMultiArray.new(C, K, P, U) { (c, k, p, u) ->
                raw[reduction.nodeValue[c, k, p, u] - 1]
            }
            val firstFired = BooleanMultiArray.new(C, U, K) { (c, u, k) ->
                raw[reduction.firstFired[c, u, k] - 1]
            }
            val notFired = BooleanMultiArray.new(C, U, K) { (c, u, k) ->
                raw[reduction.notFired[c, u, k] - 1]
            }

            println("[.] satisfaction: ${satisfaction.values.joinToString(" ", "[", "]") { it.toString() }}")

            println("[*] Satisfaction for negative scenarios:")
            for ((j, scenario) in negTree.counterExamples.withIndex()) {
                val sat = scenario.elements.joinToString(" ") { elem ->
                    elem.nodeId?.let { id ->
                        val v = satisfaction[id]
                        if (negTree.isLoopBack(v)) "<$v>" else "$v"
                    } ?: "?"
                }
                println("[${j + 1}/${negTree.counterExamples.size}] satisfaction = [$sat]")
            }

            return NegativeAssignment(
                negTree, C, K, P,
                satisfaction, actualTransition,
                nodeValue, firstFired, notFired
            )
        }
    }
}
