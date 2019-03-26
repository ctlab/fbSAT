package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.automaton.Algorithm
import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.automaton.NodeType
import ru.ifmo.fbsat.automaton.ParseTreeGuard
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.multiarray.BooleanMultiArray
import ru.ifmo.multiarray.IntMultiArray
import ru.ifmo.multiarray.MultiArray

internal class BaseAssignment(
    val scenarioTree: ScenarioTree,
    val C: Int,
    val K: Int,
    val P: Int,
    val T: Int,
    val N: Int,
    val color: IntMultiArray, // [V] : 1..C
    val transition: IntMultiArray, // [C, K] : 0..C
    val actualTransition: IntMultiArray, // [C, E, U] : 0..C
    val inputEvent: IntMultiArray, // [C, K] : 0..E
    val outputEvent: IntMultiArray, // [C] : 1..O
    val algorithm: MultiArray<Algorithm>, // [C]: Algorithm
    val nodeType: MultiArray<NodeType>, // [C, K, P] : NodeType
    val terminal: IntMultiArray, // [C, K, P] : 0..X
    val parent: IntMultiArray, // [C, K, P] : 0..P
    val childLeft: IntMultiArray, // [C, K, P] : 0..P
    val childRight: IntMultiArray, // [C, K, P] : 0..P
    val nodeValue: BooleanMultiArray, // [C, K, P, U] : Boolean
    val childValueLeft: BooleanMultiArray, // [C, K, P, U] : Boolean
    val childValueRight: BooleanMultiArray, // [C, K, P, U] : Boolean
    val firstFired: IntMultiArray, // [C, U] : 0..K
    val notFired: BooleanMultiArray // [C, U, K] : Boolean
) {
    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(
            raw: BooleanArray,
            reduction: BaseReduction
        ): BaseAssignment {
            val scenarioTree = reduction.scenarioTree
            // Constants
            val C = reduction.C
            val K = reduction.K
            val P = reduction.P
            val V = scenarioTree.size
            val E = scenarioTree.inputEvents.size
            val O = scenarioTree.outputEvents.size
            val U = scenarioTree.uniqueInputs.size
            val X = scenarioTree.uniqueInputs.first().length
            val Z = scenarioTree.uniqueOutputs.first().length
            // Automaton variables
            val color = IntMultiArray.new(V) { (v) ->
                (1..C).firstOrNull { c -> raw[reduction.color[v, c] - 1] }
                    ?: error("color[v = $v] is undefined")
            }
            val transition = IntMultiArray.new(C, K) { (i, k) ->
                (1..C).firstOrNull { j -> raw[reduction.transition[i, k, j] - 1] }
                    ?: 0
            }
            val actualTransition = IntMultiArray.new(C, E, U) { (i, e, u) ->
                (1..C).firstOrNull { j -> raw[reduction.actualTransition[i, e, u, j] - 1] }
                    ?: 0
            }
            val inputEvent = IntMultiArray.new(C, K) { (c, k) ->
                (1..E).firstOrNull { e -> raw[reduction.inputEvent[c, k, e] - 1] }
                    ?: 0
            }
            val outputEvent = IntMultiArray.new(C) { (c) ->
                (1..O).firstOrNull { o -> raw[reduction.outputEvent[c, o] - 1] }
                    ?: error("outputEvent[c = $c] is undefined")
            }
            val algorithm = MultiArray.new<Algorithm>(C) { (c) ->
                BinaryAlgorithm(
                    // Note: c is 1-based, z is 0-based
                    algorithm0 = BooleanArray(Z) { z -> raw[reduction.algorithm0[c, z + 1] - 1] },
                    algorithm1 = BooleanArray(Z) { z -> raw[reduction.algorithm1[c, z + 1] - 1] }
                )
            }
            // Guards variables
            val nodeType = MultiArray.new<NodeType>(C, K, P) { (c, k, p) ->
                NodeType.values().firstOrNull { nt -> raw[reduction.nodeType[c, k, p, nt.value] - 1] }
                    ?: error("nodeType[c,k,p = $c,$k,$p] is undefined")
            }
            val terminal = IntMultiArray.new(C, K, P) { (c, k, p) ->
                (1..X).firstOrNull { x -> raw[reduction.terminal[c, k, p, x] - 1] }
                    ?: 0
            }
            val parent = IntMultiArray.new(C, K, P) { (c, k, p) ->
                (1..P).firstOrNull { ch -> raw[reduction.parent[c, k, p, ch] - 1] }
                    ?: 0
            }
            val childLeft = IntMultiArray.new(C, K, P) { (c, k, p) ->
                (1..P).firstOrNull { ch -> raw[reduction.childLeft[c, k, p, ch] - 1] }
                    ?: 0
            }
            val childRight = IntMultiArray.new(C, K, P) { (c, k, p) ->
                (1..P).firstOrNull { ch -> raw[reduction.childRight[c, k, p, ch] - 1] }
                    ?: 0
            }
            val nodeValue = BooleanMultiArray.new(C, K, P, U) { (c, k, p, u) ->
                raw[reduction.nodeValue[c, k, p, u] - 1]
            }
            val childValueLeft = BooleanMultiArray.new(C, K, P, U) { (c, k, p, u) ->
                raw[reduction.childValueLeft[c, k, p, u] - 1]
            }
            val childValueRight = BooleanMultiArray.new(C, K, P, U) { (c, k, p, u) ->
                raw[reduction.childValueRight[c, k, p, u] - 1]
            }
            val firstFired = IntMultiArray.new(C, U) { (c, u) ->
                (1..K).firstOrNull { k -> raw[reduction.firstFired[c, u, k] - 1] }
                    ?: 0
            }
            val notFired = BooleanMultiArray.new(C, U, K) { (c, u, k) ->
                raw[reduction.notFired[c, u, k] - 1]
            }
            // Number of transitions:
            val T = transition.values.count { it != 0 }
            // Number of nodes (total guards size):
            val N = nodeType.values.count { it != NodeType.NONE }

            return BaseAssignment(
                scenarioTree, C, K, P, T, N,
                color, transition, actualTransition, inputEvent, outputEvent, algorithm,
                nodeType, terminal, parent, childLeft, childRight,
                nodeValue, childValueLeft, childValueRight, firstFired, notFired
            )
        }
    }
}

@Suppress("LocalVariableName")
internal fun BaseAssignment.toAutomaton(): Automaton {
    val automaton = Automaton(scenarioTree)

    for (c in 1..C)
        automaton.addState(
            id = c,
            outputEvent = scenarioTree.outputEvents[outputEvent[c] - 1],
            algorithm = algorithm[c]
        )

    for (c in 1..C)
        for (k in 1..K)
            if (transition[c, k] != 0)
                automaton.addTransition(
                    sourceId = c,
                    destinationId = transition[c, k],
                    inputEvent = scenarioTree.inputEvents[inputEvent[c, k] - 1],
                    guard = ParseTreeGuard(
                        nodeType = MultiArray.new<NodeType>(P) { (p) -> nodeType[c, k, p] },
                        terminal = IntMultiArray.new(P) { (p) -> terminal[c, k, p] },
                        parent = IntMultiArray.new(P) { (p) -> parent[c, k, p] },
                        childLeft = IntMultiArray.new(P) { (p) -> childLeft[c, k, p] },
                        childRight = IntMultiArray.new(P) { (p) -> childRight[c, k, p] },
                        inputNames = scenarioTree.inputNames
                    )
                )

    return automaton
}
