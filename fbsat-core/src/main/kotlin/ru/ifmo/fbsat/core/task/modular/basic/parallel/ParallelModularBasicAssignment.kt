package ru.ifmo.fbsat.core.task.modular.basic.parallel

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.Algorithm
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.ModularAutomaton
import ru.ifmo.fbsat.core.automaton.TruthTableGuard
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.utils.TheAssignment

@Suppress("PropertyName")
class ParallelModularBasicAssignment(
    val scenarioTree: ScenarioTree,
    val M: Int,
    val C: Int,
    val K: Int,
    // val modularColor: MultiArray<IntMultiArray>, // [M] : ([V] : 1..C)
    val transition: MultiArray<IntMultiArray>, // [M] : ([C, K] : 0..C)
    val actualTransition: MultiArray<IntMultiArray>, // [M] : ([C, E, U] : 0..C)
    val inputEvent: MultiArray<IntMultiArray>, // [M] : ([C, K] : 0..E)
    val outputEvent: MultiArray<IntMultiArray>, // [M] : ([C] : 0..O)
    val outputVariableModule: IntMultiArray, // [Z] : 1..M
    val algorithm: MultiArray<MultiArray<Algorithm>>, // [M] : ([C]: Algorithm)
    val rootValue: MultiArray<BooleanMultiArray>, // [M] : ([C, K, U] : Boolean)
    val firstFired: MultiArray<IntMultiArray>, // [M] : ([C, U] : 0..K)
    val notFired: MultiArray<BooleanMultiArray> // [M] : ([C, U, K] : Boolean)
) {
    val T: Int = transition.flatMap { it.values }.count { it != 0 }

    val Z: Int = outputVariableModule.shape[0]
    val moduleOutputVariables: MultiArray<List<Int>> = // [M] : {1..Z}
        MultiArray.create(M) { (m) ->
            (1..Z).filter { z -> outputVariableModule[z] == m }
        }

    companion object : TheAssignment {
        @Suppress("LocalVariableName")
        fun fromRaw(raw: BooleanArray, vars: ParallelModularBasicVariables): ParallelModularBasicAssignment {
            return with(vars) {
                ParallelModularBasicAssignment(
                    scenarioTree = scenarioTree,
                    M = M,
                    C = C,
                    K = K,
                    // modularColor = MultiArray.create(M) { (m) ->
                    //     IntMultiArray.create(V) {(v) -> compositeColor[]}
                    // },
                    transition = MultiArray.create(M) { (m) ->
                        raw.convertIntArray(modularTransitionDestination[m], C, K, domain = 1..C) { 0 }
                    },
                    actualTransition = MultiArray.create(M) { (m) ->
                        raw.convertIntArray(modularActualTransitionFunction[m], C, E, U, domain = 1..C) { 0 }
                    },
                    inputEvent = MultiArray.create(M) { (m) ->
                        raw.convertIntArray(modularTransitionInputEvent[m], C, K, domain = 1..E) { 0 }
                    },
                    outputEvent = MultiArray.create(M) { (m) ->
                        raw.convertIntArray(modularStateOutputEvent[m], C, domain = 1..O) { 0 }
                    },
                    outputVariableModule = raw.convertIntArray(
                        moduleControllingOutputVariable,
                        Z,
                        domain = 1..M
                    ) { (z) ->
                        error("outputVariableModule[z = $z] is undefined")
                    },
                    algorithm = MultiArray.create(M) { (m) ->
                        MultiArray.create(C) { (c) ->
                            BinaryAlgorithm(
                                // Note: c is 1-based, z is 0-based
                                algorithm0 = BooleanArray(Z) { z -> raw[modularStateAlgorithmBot[m][c, z + 1] - 1] },
                                algorithm1 = BooleanArray(Z) { z -> raw[modularStateAlgorithmTop[m][c, z + 1] - 1] }
                            ) as Algorithm
                        }
                    },
                    rootValue = MultiArray.create(M) { (m) ->
                        raw.convertBooleanArray(modularTransitionFiring[m], C, K, U)
                    },
                    firstFired = MultiArray.create(M) { (m) ->
                        raw.convertIntArray(modularFirstFired[m], C, U, domain = 1..K) { 0 }
                    },
                    notFired = MultiArray.create(M) { (m) ->
                        raw.convertBooleanArray(modularNotFired[m], C, K, U)
                    }
                )
            }
        }
    }
}

@Suppress("LocalVariableName")
fun ParallelModularBasicAssignment.toAutomaton(): ModularAutomaton {
    val modules = MultiArray.create(M) { (m) ->
        val automaton = Automaton(
            scenarioTree.inputEvents,
            scenarioTree.outputEvents,
            scenarioTree.inputNames,
            moduleOutputVariables[m].map { z -> scenarioTree.outputNames[z - 1] }
        )

        for (c in 1..C)
            automaton.addState(
                id = c,
                outputEvent = outputEvent[m][c].let { o ->
                    if (o == 0) null else scenarioTree.outputEvents[o - 1]
                },
                algorithm = algorithm[m][c].let { algo ->
                    algo as BinaryAlgorithm
                    val algo0 = moduleOutputVariables[m].map { z -> algo.algorithm0[z - 1] }
                    val algo1 = moduleOutputVariables[m].map { z -> algo.algorithm1[z - 1] }
                    BinaryAlgorithm(algo0, algo1)
                }
            )

        for (c in 1..C)
            for (k in 1..K)
                if (transition[m][c, k] != 0)
                    automaton.addTransition(
                        sourceId = c,
                        destinationId = transition[m][c, k],
                        inputEvent = scenarioTree.inputEvents[inputEvent[m][c, k] - 1],
                        guard = TruthTableGuard(
                            truthTable = (1..scenarioTree.uniqueInputs.size)
                                .asSequence()
                                .associate { u ->
                                    scenarioTree.uniqueInputs[u - 1] to
                                        when {
                                            notFired[m][c, k, u] -> false
                                            firstFired[m][c, u] == k -> true
                                            else -> null
                                        }
                                },
                            inputNames = scenarioTree.inputNames,
                            uniqueInputs = scenarioTree.uniqueInputs
                        )
                    )
        automaton
    }

    return ModularAutomaton(modules, outputVariableModule)
}
