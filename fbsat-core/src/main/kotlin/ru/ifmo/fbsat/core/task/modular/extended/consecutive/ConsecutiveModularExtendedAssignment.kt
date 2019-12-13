package ru.ifmo.fbsat.core.task.modular.extended.consecutive

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.Algorithm
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.automaton.ParseTreeGuard
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.utils.TheAssignment

@Suppress("PropertyName")
class ConsecutiveModularExtendedAssignment(
    val scenarioTree: ScenarioTree,
    val M: Int,
    val C: Int,
    val K: Int,
    val P: Int,
    val mapping: MultiArray<IntMultiArray>, // [M] : ([V] : 1..C)
    val transition: MultiArray<IntMultiArray>, // [M] : ([C, K] : 0..C)
    val actualTransition: MultiArray<IntMultiArray>, // [M] : ([C, E, U] : 0..C)
    val inputEvent: MultiArray<IntMultiArray>, // [M] : ([C, K] : 0..E)
    val outputEvent: MultiArray<IntMultiArray>, // [M] : ([C] : 0..O)
    val algorithm: MultiArray<MultiArray<Algorithm>>, // [M] : ([C]: Algorithm)
    val nodeType: MultiArray<MultiArray<NodeType>>, // [M] : ([C, K, P] : NodeType)
    val terminal: MultiArray<IntMultiArray>, // [M] : ([C, K, P] : 0..X)
    val parent: MultiArray<IntMultiArray>, // [M] : ([C, K, P] : 0..P)
    val child: MultiArray<IntMultiArray>, // [M] : ([C, K, P] : 0..P)
    val nodeValue: MultiArray<BooleanMultiArray>, // [M] : ([C, K, P, U] : Boolean)
    val rootValue: MultiArray<BooleanMultiArray>, // [M] : ([C, K, U] : Boolean)
    val firstFired: MultiArray<IntMultiArray>, // [M] : ([C, U] : 0..K)
    val notFired: MultiArray<BooleanMultiArray> // [M] : ([C, U, K] : Boolean)
) {
    val T: Int = transition.flatMap { it.values }.count { it != 0 }

    companion object : TheAssignment {
        @Suppress("LocalVariableName")
        fun fromRaw(
            raw: BooleanArray,
            vars: ConsecutiveModularExtendedVariables
        ): ConsecutiveModularExtendedAssignment = with(vars) {
            ConsecutiveModularExtendedAssignment(
                scenarioTree = scenarioTree,
                M = M,
                C = C,
                K = K,
                P = P,
                mapping = MultiArray.create(M) { (m) ->
                    raw.convertIntArray(modularMapping[m], V, domain = 1..C) { (v) ->
                        error("mapping[v = $v] is undefined")
                    }
                },
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
                nodeType = MultiArray.create(M) { (m) ->
                    MultiArray.create(C, K, P) { (c, k, p) ->
                        NodeType.values().firstOrNull { nt ->
                            modularNodeType[m][c, k, p, nt.value].let { t ->
                                when (t) {
                                    Solver.trueVariable -> true
                                    Solver.falseVariable -> false
                                    else -> raw[t - 1]
                                }
                            }
                        } ?: error("nodeType[c,k,p = $c,$k,$p] is undefined")
                    }
                },
                terminal = MultiArray.create(M) { (m) ->
                    raw.convertIntArray(modularNodeInputVariable[m], C, K, P, domain = 1..X) { 0 }
                },
                parent = MultiArray.create(M) { (m) ->
                    raw.convertIntArray(modularNodeParent[m], C, K, P, domain = 1..P) { 0 }
                },
                child = MultiArray.create(M) { (m) ->
                    raw.convertIntArray(modularNodeChild[m], C, K, P, domain = 1..P) { 0 }
                },
                nodeValue = MultiArray.create(M) { (m) ->
                    raw.convertBooleanArray(modularNodeValue[m], C, K, P, U)
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

@Suppress("LocalVariableName")
fun ConsecutiveModularExtendedAssignment.toAutomaton(): ConsecutiveModularAutomaton {
    val modules = MultiArray.create(M) { (m) ->
        val automaton = Automaton(scenarioTree)

        for (c in 1..C)
            automaton.addState(
                id = c,
                outputEvent = outputEvent[m][c].let {
                    if (it == 0) null else scenarioTree.outputEvents[it - 1]
                },
                algorithm = algorithm[m][c]
            )

        for (c in 1..C)
            for (k in 1..K)
                if (transition[m][c, k] != 0)
                    automaton.addTransition(
                        sourceId = c,
                        destinationId = transition[m][c, k],
                        inputEvent = scenarioTree.inputEvents[inputEvent[m][c, k] - 1],
                        guard = ParseTreeGuard(
                            nodeType = MultiArray.create(P) { (p) -> nodeType[m][c, k, p] },
                            terminal = IntMultiArray.create(P) { (p) -> terminal[m][c, k, p] },
                            parent = IntMultiArray.create(P) { (p) -> parent[m][c, k, p] },
                            childLeft = IntMultiArray.create(P) { (p) -> child[m][c, k, p] },
                            childRight = IntMultiArray.create(P) { (p) ->
                                if (nodeType[m][c, k, p] in setOf(NodeType.AND, NodeType.OR))
                                    child[m][c, k, p] + 1
                                else
                                    0
                            },
                            inputNames = scenarioTree.inputNames
                        )
                    )

        automaton
    }

    return ConsecutiveModularAutomaton(modules, scenarioTree)
}
