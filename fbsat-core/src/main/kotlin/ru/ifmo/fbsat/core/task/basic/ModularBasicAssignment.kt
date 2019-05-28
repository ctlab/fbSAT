package ru.ifmo.fbsat.core.task.basic

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.Algorithm
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.ModularAutomaton
import ru.ifmo.fbsat.core.automaton.TruthTableGuard
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment

@Suppress("PropertyName")
internal class ModularBasicAssignment(
    val scenarioTree: ScenarioTree,
    val M: Int,
    val C: Int,
    val K: Int,
    val color: IntMultiArray, // [M, V] : 1..C
    val transition: IntMultiArray, // [M, C, K] : 0..C
    val actualTransition: IntMultiArray, // [M, C, E, U] : 0..C
    val inputEvent: IntMultiArray, // [M, C, K] : 0..E
    val outputEvent: IntMultiArray, // [M, C] : 1..O
    val outputVariableModule: IntMultiArray, // [Z] : 1..M
    val algorithm: MultiArray<Algorithm>, // [M, C]: Algorithm
    val rootValue: BooleanMultiArray, // [M, C, K, U] : Boolean
    val firstFired: IntMultiArray, // [M, C, U] : 0..K
    val notFired: BooleanMultiArray // [M, C, U, K] : Boolean
) {
    val T: Int = transition.values.count { it != 0 }
    val Z: Int = outputVariableModule.shape[0]
    val moduleOutputVariables: MultiArray<List<Int>> = // [M] : {1..Z}
        MultiArray.create(M) { (m) ->
            (1..Z).filter { z -> outputVariableModule[z] == m }
        }

    init {
        // for (z in 1..Z)
        //     println("output variable z = $z (${scenarioTree.outputNames[z - 1]}) is controlled by module m = ${outputVariableModule[z]}")

        // for (m in 1..M)
        //     println("output variables in module m = $m: ${moduleOutputVariables[m].map { z -> scenarioTree.outputNames[z - 1] }}")
    }

    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(raw: RawAssignment): ModularBasicAssignment {
            // Constants
            val scenarioTree: ScenarioTree by raw
            val M: Int by raw
            val C: Int by raw
            val K: Int by raw
            val V: Int by raw
            val E: Int by raw
            val O: Int by raw
            val U: Int by raw
            val Z: Int by raw
            // Reduction variables
            val transition: IntMultiArray by raw
            val actualTransition: IntMultiArray by raw
            val inputEvent: IntMultiArray by raw
            val outputEvent: IntMultiArray by raw
            val outputVariableModule: IntMultiArray by raw
            val algorithm0: IntMultiArray by raw
            val algorithm1: IntMultiArray by raw
            val color: IntMultiArray by raw
            val rootValue: IntMultiArray by raw
            val firstFired: IntMultiArray by raw
            val notFired: IntMultiArray by raw

            return ModularBasicAssignment(
                scenarioTree = scenarioTree,
                M = M,
                C = C,
                K = K,
                color = raw.intArray(color, M, V, domain = 1..C) { (m, v) ->
                    error("color[m = $m, v = $v] is undefined")
                },
                transition = raw.intArray(transition, M, C, K, domain = 1..C) { 0 },
                actualTransition = raw.intArray(actualTransition, M, C, E, U, domain = 1..C) { 0 },
                inputEvent = raw.intArray(inputEvent, M, C, K, domain = 1..E) { 0 },
                outputEvent = raw.intArray(outputEvent, M, C, domain = 1..O) { (m, c) ->
                    error("outputEvent[m = $m, c = $c] is undefined")
                },
                outputVariableModule = raw.intArray(outputVariableModule, Z, domain = 1..M) { (z) ->
                    error("outputVariableModule[z = $z] is undefined")
                },
                algorithm = MultiArray.create(M, C) { (m, c) ->
                    BinaryAlgorithm(
                        // Note: c is 1-based, z is 0-based
                        algorithm0 = BooleanArray(Z) { z -> raw[algorithm0[m, c, z + 1]] },
                        algorithm1 = BooleanArray(Z) { z -> raw[algorithm1[m, c, z + 1]] }
                    )
                },
                rootValue = raw.booleanArray(rootValue, M, C, K, U),
                firstFired = raw.intArray(firstFired, M, C, U, domain = 1..K) { 0 },
                notFired = raw.booleanArray(notFired, M, C, U, K)
            )
        }
    }
}

@Suppress("LocalVariableName")
internal fun ModularBasicAssignment.toAutomaton(): ModularAutomaton {
    val modules = MultiArray.create(M) { (m) ->
        val automaton = Automaton(
            scenarioTree.inputEvents,
            scenarioTree.outputEvents,
            scenarioTree.inputNames,
            moduleOutputVariables[m].map { z -> scenarioTree.outputNames[z - 1] }
        )

        for (c in 1..C)
            automaton.addState(
                c,
                scenarioTree.outputEvents[outputEvent[m, c] - 1],
                algorithm[m, c].let { algo ->
                    algo as BinaryAlgorithm
                    val algo0 = moduleOutputVariables[m].map { z -> algo.algorithm0[z - 1] }
                    val algo1 = moduleOutputVariables[m].map { z -> algo.algorithm1[z - 1] }
                    BinaryAlgorithm(algo0, algo1)
                }
            )

        for (c in 1..C)
            for (k in 1..K)
                if (transition[m, c, k] != 0)
                    automaton.addTransition(
                        sourceId = c,
                        destinationId = transition[m, c, k],
                        inputEvent = scenarioTree.inputEvents[inputEvent[m, c, k] - 1],
                        guard = TruthTableGuard(
                            truthTable = (1..scenarioTree.uniqueInputs.size)
                                .asSequence()
                                .associateWith { u ->
                                    when {
                                        notFired[m, c, u, k] -> false
                                        firstFired[m, c, u] == k -> true
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
        automaton
    }

    return ModularAutomaton(modules, outputVariableModule)
}
