package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.Algorithm
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.single.basic.BasicAssignment
import ru.ifmo.fbsat.core.task.single.basic.toAutomaton
import ru.ifmo.fbsat.core.utils.TheAssignment

@Suppress("PropertyName")
class ConsecutiveModularBasicAssignment(
    val scenarioTree: ScenarioTree,
    val M: Int,
    val C: Int,
    val K: Int,
    val transition: MultiArray<IntMultiArray>, // [M] : ([C, K] : 0..C)
    val actualTransition: MultiArray<IntMultiArray>, // [M] : ([C, E, U] : 0..C)
    val inputEvent: MultiArray<IntMultiArray>, // [M] : ([C, K] : 0..E)
    val outputEvent: MultiArray<IntMultiArray>, // [M] : ([C] : 0..O)
    val algorithm: MultiArray<MultiArray<Algorithm>>, // [M] : ([C]: Algorithm)
    val rootValue: MultiArray<BooleanMultiArray>, // [M] : ([C, K, U] : Boolean)
    val firstFired: MultiArray<IntMultiArray>, // [M] : ([C, U] : 0..K)
    val notFired: MultiArray<BooleanMultiArray>, // [M] : ([C, U, K] : Boolean)
    val mapping: MultiArray<IntMultiArray> // [M] : ([V] : 1..C)
) {
    val T: Int = transition.flatMap { it.values }.count { it != 0 }

    /* Modularized BasicAssignment */
    val modularBasicAssignment: MultiArray<BasicAssignment> =
        MultiArray.create(M) { (m) ->
            BasicAssignment(
                scenarioTree,
                C, K,
                mapping = mapping[m],
                transition = transition[m],
                actualTransition = actualTransition[m],
                inputEvent = inputEvent[m],
                outputEvent = outputEvent[m],
                stateAlgorithm = algorithm[m],
                transitionFiring = rootValue[m],
                firstFired = firstFired[m],
                notFired = notFired[m]
            )
        }

    companion object : TheAssignment {
        @Suppress("LocalVariableName")
        fun fromRaw(raw: BooleanArray, vars: ConsecutiveModularBasicVariables): ConsecutiveModularBasicAssignment {
            return with(vars) {
                ConsecutiveModularBasicAssignment(
                    scenarioTree = scenarioTree,
                    M = M,
                    C = C,
                    K = K,
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
                    },
                    mapping = MultiArray.create(M) { (m) ->
                        raw.convertIntArray(modularMapping[m], V, domain = 1..C) { (v) ->
                            error("mapping[m = $m, v = $v] is undefined")
                        }
                    }
                    // .also {
                    //     println("ConsecutiveModularBasicAssignment :: mapping:")
                    //     println("      ${(1..V).joinPadded(2)}")
                    //     for (m in 1..M) {
                    //         println("[m=$m] ${it[m].joinPadded(2)}")
                    //     }
                    // }
                )
            }
        }
    }
}

@Suppress("LocalVariableName")
fun ConsecutiveModularBasicAssignment.toAutomaton(): ConsecutiveModularAutomaton {
    val modules = MultiArray.create(M) { (m) ->
        modularBasicAssignment[m].toAutomaton()
    }

    return ConsecutiveModularAutomaton(modules, scenarioTree)
}
