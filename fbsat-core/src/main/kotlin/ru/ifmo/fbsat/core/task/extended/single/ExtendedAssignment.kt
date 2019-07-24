package ru.ifmo.fbsat.core.task.extended.single

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.Algorithm
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.automaton.ParseTreeGuard
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.complete.single.CompleteVariables
import ru.ifmo.fbsat.core.utils.TheAssignment

class ExtendedAssignment(
    val scenarioTree: ScenarioTree,
    val C: Int,
    val K: Int,
    val P: Int,
    val color: IntMultiArray, // [V] : 1..C
    val transition: IntMultiArray, // [C, K] : 0..C
    val actualTransition: IntMultiArray, // [C, E, U] : 0..C
    val inputEvent: IntMultiArray, // [C, K] : 0..E
    val outputEvent: IntMultiArray, // [C] : 0..O
    val algorithm: MultiArray<Algorithm>, // [C]: Algorithm
    val nodeType: MultiArray<NodeType>, // [C, K, P] : NodeType
    val terminal: IntMultiArray, // [C, K, P] : 0..X
    val parent: IntMultiArray, // [C, K, P] : 0..P
    val child: IntMultiArray, // [C, K, P] : 0..P
    val nodeValue: BooleanMultiArray, // [C, K, P, U] : Boolean
    val rootValue: BooleanMultiArray, // [C, K, U] : Boolean
    val firstFired: IntMultiArray, // [C, U] : 0..K
    val notFired: BooleanMultiArray // [C, U, K] : Boolean
) {
    /**
     * Number of transitions.
     */
    @Suppress("PropertyName")
    val T: Int = transition.values.count { it != 0 }
    /**
     * Total guards size (total number of nodes in all parse trees).
     */
    @Suppress("PropertyName")
    val N: Int = nodeType.values.count { it != NodeType.NONE }

    companion object : TheAssignment {
        fun fromRaw(raw: BooleanArray, vars: ExtendedVariables): ExtendedAssignment {
            return with(vars) {
                ExtendedAssignment(
                    scenarioTree = scenarioTree,
                    C = C,
                    K = K,
                    P = P,
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
                    nodeType = MultiArray.create(C, K, P) { (c, k, p) ->
                        NodeType.values().firstOrNull { nt ->
                            nodeType[c, k, p, nt.value].let { t ->
                                when (t) {
                                    Solver.trueVariable -> true
                                    Solver.falseVariable -> false
                                    else -> raw[t - 1]
                                }
                            }
                        } ?: error("nodeType[c,k,p = $c,$k,$p] is undefined")
                    },
                    terminal = raw.convertIntArray(terminal, C, K, P, domain = 1..X) { 0 },
                    parent = raw.convertIntArray(parent, C, K, P, domain = 1..P) { 0 },
                    child = raw.convertIntArray(child, C, K, P, domain = 1..P) { 0 },
                    nodeValue = raw.convertBooleanArray(nodeValue, C, K, P, U),
                    rootValue = raw.convertBooleanArray(rootValue, C, K, U),
                    firstFired = raw.convertIntArray(firstFired, C, U, domain = 1..K) { 0 },
                    notFired = raw.convertBooleanArray(notFired, C, U, K)
                )
            }
        }

        fun fromRaw(raw: BooleanArray, vars: CompleteVariables): ExtendedAssignment {
            with(vars) {
                return fromRaw(raw, extVars)
            }
        }
    }
}

@Suppress("LocalVariableName")
fun ExtendedAssignment.toAutomaton(): Automaton {
    val automaton = Automaton(scenarioTree)

    for (c in 1..C)
        automaton.addState(
            id = c,
            outputEvent = outputEvent[c].let {
                if (it == 0) null else scenarioTree.outputEvents[it - 1]
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
                    guard = ParseTreeGuard(
                        nodeType = MultiArray.create(P) { (p) -> nodeType[c, k, p] },
                        terminal = IntMultiArray.create(P) { (p) -> terminal[c, k, p] },
                        parent = IntMultiArray.create(P) { (p) -> parent[c, k, p] },
                        childLeft = IntMultiArray.create(P) { (p) -> child[c, k, p] },
                        childRight = IntMultiArray.create(P) { (p) ->
                            if (nodeType[c, k, p] in setOf(NodeType.AND, NodeType.OR))
                                child[c, k, p] + 1
                            else
                                0
                        },
                        inputNames = scenarioTree.inputNames
                    )
                )

    return automaton
}
