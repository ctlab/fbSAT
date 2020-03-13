package ru.ifmo.fbsat.core.task.single.extforest

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.automaton.ParseTreeGuard
import ru.ifmo.fbsat.core.automaton.endow
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.solver.convert
import java.util.ArrayDeque
import java.util.Deque

class ExtForestAssignment(
    val scenarioTree: ScenarioTree,
    val C: Int,
    val K: Int,
    val P: Int,
    val V: Int,
    val E: Int,
    val O: Int,
    val X: Int,
    val Z: Int,
    val U: Int,
    /* Core variables */
    val actualTransitionFunction: IntMultiArray, // [C, E, U] : [0..C]
    val transitionDestination: IntMultiArray, // [C, K] : 0..C
    val transitionInputEvent: IntMultiArray, // [C, K]  0..E
    val transitionFiring: BooleanMultiArray, // [C, K, U] : Boolean
    val firstFired: IntMultiArray, // [C, U] : 0..K
    val notFired: BooleanMultiArray, // [C, K, U] : Boolean
    val stateOutputEvent: IntMultiArray, // [C] : 0..O
    val stateAlgorithmTop: BooleanMultiArray, // [C, Z] : Boolean
    val stateAlgorithmBot: BooleanMultiArray, // [C, Z] : Boolean
    /* Mapping variables */
    val mapping: IntMultiArray, // [V] : 1..C
    /* Guard conditions variables */
    val nodeType: MultiArray<NodeType>, // [P] : NodeType
    val nodeInputVariable: IntMultiArray, // [P] : 0..X
    val nodeParent: IntMultiArray, // [P] : 0..P
    val nodeChild: IntMultiArray, // [P] : 0..P
    val nodeValue: BooleanMultiArray // [P, U] : Boolean
) {
    /**
     * Number of transitions.
     */
    @Suppress("PropertyName")
    val T: Int = transitionDestination.values.count { it != 0 }

    /**
     * Total guards size (total number of typed nodes in all parse trees).
     */
    @Suppress("PropertyName")
    val N: Int = nodeType.values.count { it != NodeType.NONE }

    companion object {
        fun fromRaw(
            raw: RawAssignment,
            vars: ExtForestVariables
        ): ExtForestAssignment = with(vars) {
            ExtForestAssignment(
                scenarioTree = scenarioTree,
                C = C, K = K, P = P,
                V = V, E = E, O = O, X = X, Z = Z, U = U,
                actualTransitionFunction = actualTransitionFunction.convert(raw),
                transitionDestination = transitionDestination.convert(raw),
                transitionInputEvent = transitionInputEvent.convert(raw),
                transitionFiring = transitionFiring.convert(raw),
                firstFired = firstFired.convert(raw),
                notFired = notFired.convert(raw),
                stateOutputEvent = stateOutputEvent.convert(raw),
                stateAlgorithmTop = stateAlgorithmTop.convert(raw),
                stateAlgorithmBot = stateAlgorithmBot.convert(raw),
                mapping = mapping.convert(raw),
                nodeType = nodeType.convert(raw),
                nodeInputVariable = nodeInputVariable.convert(raw),
                nodeParent = nodeParent.convert(raw),
                nodeChild = nodeChild.convert(raw),
                nodeValue = nodeValue.convert(raw)
            )
        }
    }
}

fun ExtForestAssignment.toAutomaton(): Automaton =
    Automaton(scenarioTree).endow(
        C = C, K = K,
        stateOutputEvent = { c ->
            stateOutputEvent[c].let { o ->
                if (o == 0) null else scenarioTree.outputEvents[o - 1]
            }
        },
        stateAlgorithm = { c ->
            BinaryAlgorithm(
                algorithm0 = BooleanArray(Z) { z0 -> stateAlgorithmBot[c, z0 + 1] },
                algorithm1 = BooleanArray(Z) { z0 -> stateAlgorithmTop[c, z0 + 1] }
            )
        },
        transitionDestination = { c, k ->
            transitionDestination[c, k]
        },
        transitionInputEvent = { c, k ->
            scenarioTree.inputEvents[transitionInputEvent[c, k] - 1]
        },
        transitionGuard = { c, k ->
            val root = ck2p(c, k, K)
            check(nodeType[root] != NodeType.NONE)
            val nodes: MutableList<Int> = mutableListOf()
            val queue: Deque<Int> = ArrayDeque(listOf(root))
            while (queue.isNotEmpty()) {
                val p = queue.removeFirst()
                nodes.add(p)
                for (ch in (C * K + 1)..P) {
                    if (nodeParent[ch] == p) {
                        queue.addLast(ch)
                    }
                }
            }
            check(nodes.isNotEmpty()) { "Empty parse tree for c = $c, k = $k" }
            val G = nodes.size
            ParseTreeGuard(
                nodeType = MultiArray.create(G) { (p) ->
                    nodeType[nodes[p - 1]]
                },
                terminal = IntMultiArray.create(G) { (p) ->
                    nodeInputVariable[nodes[p - 1]]
                },
                parent = IntMultiArray.create(G) { (p) ->
                    nodeParent[nodes[p - 1]].let { if (it != 0) nodes.indexOf(it) + 1 else 0 }
                },
                childLeft = IntMultiArray.create(G) { (p) ->
                    nodeChild[nodes[p - 1]].let { if (it != 0) nodes.indexOf(it) + 1 else 0 }
                },
                childRight = IntMultiArray.create(G) { (p) ->
                    if (nodeType[nodes[p - 1]] in setOf(NodeType.AND, NodeType.OR))
                        nodes.indexOf(nodeChild[nodes[p - 1]]) + 2
                    else
                        0
                },
                inputNames = scenarioTree.inputNames
            )
        }
    )
