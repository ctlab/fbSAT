package ru.ifmo.fbsat.task.basic

import ru.ifmo.fbsat.automaton.Algorithm
import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.automaton.TruthTableGuard
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.utils.BooleanMultiArray
import ru.ifmo.fbsat.utils.IntMultiArray
import ru.ifmo.fbsat.utils.MultiArray

internal class Assignment(
    val scenarioTree: ScenarioTree,
    val C: Int,
    val K: Int,
    // val T: Int,
    val color: IntMultiArray,
    val transition: IntMultiArray,
    val outputEvent: IntMultiArray,
    val algorithm: MultiArray<Algorithm>,
    val firstFired: BooleanMultiArray,
    val notFired: BooleanMultiArray
) {
    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(baseReduction: Reduction, raw: BooleanArray): Assignment {
            val scenarioTree = baseReduction.scenarioTree
            // Constants
            val C = baseReduction.C
            val K = baseReduction.K
            val V = scenarioTree.size
            val E = scenarioTree.inputEvents.size
            val O = scenarioTree.outputEvents.size
            val U = scenarioTree.uniqueInputs.size
            val Z = scenarioTree.uniqueOutputs.first().length
            // Automaton variables
            val color = IntMultiArray.new(V) { (v) ->
                (1..C).firstOrNull { c -> raw[baseReduction.color[v, c] - 1] }
                    ?: throw IllegalStateException("color[v = $v] is undefined")
            }
            val transition = IntMultiArray.new(C, E, K) { (i, e, k) ->
                (1..C).firstOrNull { j -> raw[baseReduction.transition[i, e, k, j] - 1] }
                    ?: 0
            }
            val outputEvent = IntMultiArray.new(C) { (c) ->
                (1..O).firstOrNull { o -> raw[baseReduction.outputEvent[c, o] - 1] }
                    ?: throw IllegalStateException("outputEvent[c = $c] is undefined")
            }
            val algorithm = MultiArray.new<Algorithm>(C) { (c) ->
                BinaryAlgorithm(
                    // Note: c is 1-based, z is 0-based
                    algorithm0 = BooleanArray(Z) { z -> raw[baseReduction.algorithm0[c, z + 1] - 1] },
                    algorithm1 = BooleanArray(Z) { z -> raw[baseReduction.algorithm1[c, z + 1] - 1] }
                )
            }
            // Guards variables
            val firstFired = BooleanMultiArray.new(C, E, U, K) { (c, e, u, k) ->
                raw[baseReduction.firstFired[c, e, u, k] - 1]
            }
            val notFired = BooleanMultiArray.new(C, E, U, K) { (c, e, u, k) ->
                raw[baseReduction.notFired[c, e, u, k] - 1]
            }
            // Number of transitions:
            // val T = transition.values.count { it != 0 }

            return Assignment(
                scenarioTree, C, K,
                color, transition, outputEvent, algorithm,
                firstFired, notFired
            )
        }
    }
}

@Suppress("LocalVariableName")
internal fun Assignment.toAutomaton(): Automaton {
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
        for (e in 1..scenarioTree.inputEvents.size)
            for (k in 1..K) {
                val dest = transition[c, e, k]
                if (dest != 0) {
                    val truthTable = (1..scenarioTree.uniqueInputs.size)
                        .asSequence()
                        .joinToString("") { u ->
                            when {
                                notFired[c, e, u, k] -> "0"
                                firstFired[c, e, u, k] -> "1"
                                else -> "x"
                            }
                        }
                    val guard = TruthTableGuard(truthTable, scenarioTree.inputNames, scenarioTree.uniqueInputs)
                    automaton.addTransition(c, dest, scenarioTree.inputEvents[e - 1], guard)
                }
            }

    return automaton
}
