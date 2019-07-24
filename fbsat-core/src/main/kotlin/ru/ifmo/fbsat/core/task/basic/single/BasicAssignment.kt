package ru.ifmo.fbsat.core.task.basic.single

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.Algorithm
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.TruthTableGuard
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.utils.TheAssignment

class BasicAssignment(
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

    companion object : TheAssignment {
        fun fromRaw(raw: BooleanArray, vars: BasicVariables): BasicAssignment {
            return with(vars) {
                BasicAssignment(
                    scenarioTree = scenarioTree,
                    C = C,
                    K = K,
                    color = raw.convertIntArray(color, V, domain = 1..C) { (v) ->
                        error("color[v = $v] is undefined")
                    },
                    transition = raw.convertIntArray(transition, C, K, domain = 1..C) { 0 },
                    actualTransition = raw.convertIntArray(actualTransition, C, E, U, domain = 1..C) { 0 },
                    inputEvent = raw.convertIntArray(inputEvent, C, K, domain = 1..E) { 0 },
                    outputEvent = raw.convertIntArray(outputEvent, C, domain = 1..O) { 0 },
                    algorithm = MultiArray.create(C) { (c) ->
                        BinaryAlgorithm(
                            // Note: c is 1-based, z is 0-based
                            algorithm0 = BooleanArray(Z) { z -> raw[algorithm0[c, z + 1] - 1] },
                            algorithm1 = BooleanArray(Z) { z -> raw[algorithm1[c, z + 1] - 1] }
                        )
                    },
                    rootValue = raw.convertBooleanArray(rootValue, C, K, U),
                    firstFired = raw.convertIntArray(firstFired, C, U, domain = 1..K) { 0 },
                    notFired = raw.convertBooleanArray(notFired, C, U, K)
                )
            }
        }
    }
}

@Suppress("LocalVariableName")
fun BasicAssignment.toAutomaton(): Automaton {
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
