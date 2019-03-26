package ru.ifmo.fbsat.task.basic

import ru.ifmo.fbsat.automaton.Algorithm
import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.automaton.TruthTableGuard
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.multiarray.BooleanMultiArray
import ru.ifmo.multiarray.IntMultiArray
import ru.ifmo.multiarray.MultiArray

internal class BaseAssignment(
    val scenarioTree: ScenarioTree,
    val C: Int,
    val K: Int,
    val T: Int,
    val color: IntMultiArray, // [V] : 1..C
    val transition: IntMultiArray, // [C, K] : 0..C
    val actualTransition: IntMultiArray, // [C, E, U] : 0..C
    val inputEvent: IntMultiArray, // [C, K] : 0..E
    val outputEvent: IntMultiArray, // [C] : 1..O
    val algorithm: MultiArray<Algorithm>, // [C]: Algorithm
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
            val firstFired = IntMultiArray.new(C, U) { (c, u) ->
                (1..K).firstOrNull { k -> raw[reduction.firstFired[c, u, k] - 1] }
                    ?: 0
            }
            val notFired = BooleanMultiArray.new(C, U, K) { (c, u, k) ->
                raw[reduction.notFired[c, u, k] - 1]
            }
            // Number of transitions:
            val T = transition.values.count { it != 0 }

            return BaseAssignment(
                scenarioTree, C, K, T,
                color, transition, actualTransition, inputEvent, outputEvent, algorithm,
                firstFired, notFired
            )
        }
    }
}

@Suppress("LocalVariableName")
internal fun BaseAssignment.toAutomaton(): Automaton {
    val automaton = Automaton(scenarioTree)

    val C = C
    val K = K

    for (c in 1..C) {
        automaton.addState(
            c,
            scenarioTree.outputEvents[outputEvent[c] - 1],
            algorithm[c]
        )
    }

    for (c in 1..C)
        for (k in 1..K)
            if (transition[c, k] != 0)
                automaton.addTransition(
                    sourceId = c,
                    destinationId = transition[c, k],
                    inputEvent = scenarioTree.inputEvents[inputEvent[c, k] - 1],
                    guard = TruthTableGuard(
                        truthTable = (1..scenarioTree.uniqueInputs.size)
                            .asSequence()
                            .joinToString("") { u ->
                                when {
                                    notFired[c, u, k] -> "0"
                                    firstFired[c, u] == k -> "1"
                                    else -> "x"
                                }
                            },
                        inputNames = scenarioTree.inputNames,
                        uniqueInputs = scenarioTree.uniqueInputs
                    )
                )

    return automaton
}
