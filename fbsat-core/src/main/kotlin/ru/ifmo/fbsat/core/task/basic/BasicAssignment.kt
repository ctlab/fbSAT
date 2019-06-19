package ru.ifmo.fbsat.core.task.basic

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.Algorithm
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.TruthTableGuard
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment

internal class BasicAssignment(
    val scenarioTree: ScenarioTree,
    val C: Int,
    val K: Int,
    val color: IntMultiArray, // [V] : 1..C
    val transition: IntMultiArray, // [C, K] : 0..C
    val actualTransition: IntMultiArray, // [C, E, U] : 0..C
    val inputEvent: IntMultiArray, // [C, K] : 0..E
    val outputEvent: IntMultiArray, // [C] : 1..O
    val algorithm: MultiArray<Algorithm>, // [C]: Algorithm
    val rootValue: BooleanMultiArray, // [C, K, U] : Boolean
    val firstFired: IntMultiArray, // [C, U] : 0..K
    val notFired: BooleanMultiArray // [C, U, K] : Boolean
) {
    @Suppress("PropertyName")
    val T: Int = transition.values.count { it != 0 }

    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(raw: RawAssignment): BasicAssignment {
            // Constants
            val scenarioTree: ScenarioTree by raw
            val C: Int by raw
            val K: Int by raw
            val V: Int by raw
            val E: Int by raw
            val O: Int by raw
            val U: Int by raw
            val X: Int by raw
            val Z: Int by raw
            // Reduction variables
            val transition: IntMultiArray by raw
            val actualTransition: IntMultiArray by raw
            val inputEvent: IntMultiArray by raw
            val outputEvent: IntMultiArray by raw
            val algorithm0: IntMultiArray by raw
            val algorithm1: IntMultiArray by raw
            val color: IntMultiArray by raw
            val rootValue: IntMultiArray by raw
            val firstFired: IntMultiArray by raw
            val notFired: IntMultiArray by raw

            return BasicAssignment(
                scenarioTree = scenarioTree,
                C = C,
                K = K,
                color = raw.intArray(color, V, domain = 1..C) { (v) ->
                    error("color[v = $v] is undefined")
                },
                transition = raw.intArray(transition, C, K, domain = 1..C) { 0 },
                actualTransition = raw.intArray(actualTransition, C, E, U, domain = 1..C) { 0 },
                inputEvent = raw.intArray(inputEvent, C, K, domain = 1..E) { 0 },
                outputEvent = raw.intArray(outputEvent, C, domain = 1..O) { 0 },
                algorithm = MultiArray.create(C) { (c) ->
                    BinaryAlgorithm(
                        // Note: c is 1-based, z is 0-based
                        algorithm0 = BooleanArray(Z) { z -> raw[algorithm0[c, z + 1]] },
                        algorithm1 = BooleanArray(Z) { z -> raw[algorithm1[c, z + 1]] }
                    )
                },
                rootValue = raw.booleanArray(rootValue, C, K, U),
                firstFired = raw.intArray(firstFired, C, U, domain = 1..K) { 0 },
                notFired = raw.booleanArray(notFired, C, U, K)
            )
        }
    }
}

@Suppress("LocalVariableName")
internal fun BasicAssignment.toAutomaton(): Automaton {
    val automaton = Automaton(scenarioTree)

    for (c in 1..C)
        automaton.addState(
            id = c,
            outputEvent = outputEvent[c].let { o ->
                if (o == 0) null else scenarioTree.outputEvents[o - 1]
            },
            algorithm = algorithm[c]
        )

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
                            .associateWith { u ->
                                when {
                                    notFired[c, u, k] -> false
                                    firstFired[c, u] == k -> true
                                    else -> null
                                }
                            }
                            .mapKeys { (u, _) ->
                                scenarioTree.uniqueInputs[u - 1]
                            },
                        inputNames = scenarioTree.inputNames,
                        uniqueInputs = scenarioTree.uniqueInputs
                    )
                )

    return automaton
}
