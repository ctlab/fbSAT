package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.automaton.NodeType
import ru.ifmo.fbsat.scenario.negative.NegativeScenarioTree
import ru.ifmo.multiarray.BooleanMultiArray
import ru.ifmo.multiarray.IntMultiArray
import ru.ifmo.multiarray.MultiArray

internal class NegativeAssignment(
    val negativeScenarioTree: NegativeScenarioTree,
    val C: Int,
    val K: Int,
    val P: Int,
    // ===
    val nodeType: MultiArray<NodeType>, // [C, K, P] : NodeType
    val terminal: IntMultiArray, // [C, K, P] : 0..X
    // ===
    val satisfaction: IntMultiArray, // [V] : 0..C
    val actualTransition: IntMultiArray, // [C, E, U] : 0..C
    val nodeValue: BooleanMultiArray, // [C, K, P, U] : Boolean
    val firstFired: IntMultiArray, // [C, U] : 0..K
    val notFired: BooleanMultiArray // [C, U, K] : Boolean
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
            val firstFired = IntMultiArray.new(C, U) { (c, u) ->
                (1..K).firstOrNull { k -> raw[reduction.firstFired[c, u, k] - 1] }
                    ?: 0
            }
            val notFired = BooleanMultiArray.new(C, U, K) { (c, u, k) ->
                raw[reduction.notFired[c, u, k] - 1]
            }

            // ===
            val nodeType = MultiArray.new<NodeType>(C, K, P) { (c, k, p) ->
                NodeType.values().firstOrNull { nt -> raw[reduction.nodeType[c, k, p, nt.value] - 1] }
                    ?: error("nodeType[c,k,p = $c,$k,$p] is undefined")
            }
            val terminal = IntMultiArray.new(C, K, P) { (c, k, p) ->
                (1..negTree.uniqueInputs.first().length).firstOrNull { x -> raw[reduction.terminal[c, k, p, x] - 1] }
                    ?: 0
            }
            // ===

            // println("[*] terminal:")
            // for (c in 1..C)
            //     for (k in 1..K)
            //         println("[.] terminal[c = $c, k = $k, p = ${1..P}] = ${(1..P).map { p -> if (terminal[c, k, p] != 0) reduction.terminal[c, k, p, terminal[c, k, p]] else 0 }} = ${(1..P).map { p -> terminal[c, k, p] }}")

            // println("[*] elem.nodeId`s for counterexamples:")
            // for ((j, scenario) in negTree.counterexamples.withIndex()) {
            //     val nodeIds = scenario.elements.mapIndexed { i, elem ->
            //         elem.nodeId?.let { id ->
            //             if (i + 1 == scenario.loopPosition) "<$id>" else "$id"
            //         } ?: "?"
            //     }.joinToString(" ")
            //     println(
            //         "[${j + 1}/${negTree.counterexamples.size}] nodeIds = [$nodeIds] (loopPosition = ${scenario.loopPosition}, loopBacks = ${negTree.loopBacks(
            //             scenario.elements.last().nodeId!!
            //         )})"
            //     )
            // }

            // println("[*] Satisfaction for negative scenarios:")
            // for ((j, scenario) in negTree.counterexamples.withIndex()) {
            //     val sat = scenario.elements.mapIndexed { i, elem ->
            //         elem.nodeId?.let { id ->
            //             val c = satisfaction[id]
            //             if (i + 1 == scenario.loopPosition) "<$c>" else "$c"
            //         } ?: "?"
            //     }.joinToString(" ")
            //     println("[${j + 1}/${negTree.counterexamples.size}] satisfaction = [$sat]")
            // }

            return NegativeAssignment(
                negTree, C, K, P,
                nodeType, terminal,
                satisfaction, actualTransition,
                nodeValue, firstFired, notFired
            )
        }
    }
}
