package ru.ifmo.fbsat.core.task.extended

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.Algorithm
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.automaton.ParseTreeGuard
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment

internal class ExtendedAssignment(
    val scenarioTree: ScenarioTree,
    val C: Int,
    val K: Int,
    val P: Int,
    val color: IntMultiArray, // [V] : 1..C
    val transition: IntMultiArray, // [C, K] : 0..C
    val actualTransition: IntMultiArray, // [C, E, U] : 0..C
    val inputEvent: IntMultiArray, // [C, K] : 0..E
    val outputEvent: IntMultiArray, // [C] : 1..O
    val algorithm: MultiArray<Algorithm>, // [C]: Algorithm
    val nodeType: MultiArray<NodeType>, // [C, K, P] : NodeType
    val terminal: IntMultiArray, // [C, K, P] : 0..X
    val parent: IntMultiArray, // [C, K, P] : 0..P
    val childLeft: IntMultiArray, // [C, K, P] : 0..P
    val childRight: IntMultiArray, // [C, K, P] : 0..P
    val nodeValue: BooleanMultiArray, // [C, K, P, U] : Boolean
    val rootValue: BooleanMultiArray, // [C, K, U] : Boolean
    val childValueLeft: BooleanMultiArray, // [C, K, P, U] : Boolean
    val childValueRight: BooleanMultiArray, // [C, K, P, U] : Boolean
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

    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(raw: RawAssignment): ExtendedAssignment {
            // Constants
            val scenarioTree: ScenarioTree by raw
            val C: Int by raw
            val K: Int by raw
            val P: Int by raw
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
            val nodeType: IntMultiArray by raw
            val terminal: IntMultiArray by raw
            val parent: IntMultiArray by raw
            val childLeft: IntMultiArray by raw
            val childRight: IntMultiArray by raw
            val nodeValue: IntMultiArray by raw
            val rootValue: IntMultiArray by raw
            val childValueLeft: IntMultiArray by raw
            val childValueRight: IntMultiArray by raw
            val firstFired: IntMultiArray by raw
            val notFired: IntMultiArray by raw

            return ExtendedAssignment(
                scenarioTree = scenarioTree,
                C = C,
                K = K,
                P = P,
                color = raw.intArrayOf(color, V, domain = 1..C) { (v) ->
                    error("color[v = $v] is undefined")
                },
                transition = raw.intArrayOf(transition, C, K, domain = 1..C) { 0 },
                actualTransition = raw.intArrayOf(actualTransition, C, E, U, domain = 1..C) { 0 },
                inputEvent = raw.intArrayOf(inputEvent, C, K, domain = 1..E) { 0 },
                outputEvent = raw.intArrayOf(outputEvent, C, domain = 1..O) { (c) ->
                    error("outputEvent[c = $c] is undefined")
                },
                algorithm = MultiArray.create(C) { (c) ->
                    BinaryAlgorithm(
                        // Note: c is 1-based, z is 0-based
                        algorithm0 = BooleanArray(Z) { z -> raw[algorithm0[c, z + 1]] },
                        algorithm1 = BooleanArray(Z) { z -> raw[algorithm1[c, z + 1]] }
                    )
                },
                nodeType = MultiArray.create(C, K, P) { (c, k, p) ->
                    NodeType.values().firstOrNull { nt ->
                        nodeType[c, k, p, nt.value].let { t -> if (t == 0) false else raw[t] }
                    } ?: error("nodeType[c,k,p = $c,$k,$p] is undefined")
                },
                terminal = raw.intArrayOf(terminal, C, K, P, domain = 1..X) { 0 },
                parent = raw.intArrayOf(parent, C, K, P, domain = 1..P) { 0 },
                childLeft = raw.intArrayOf(childLeft, C, K, P, domain = 1..P) { 0 },
                childRight = raw.intArrayOf(childRight, C, K, P, domain = 1..P) { 0 },
                nodeValue = raw.booleanArrayOf(nodeValue, C, K, P, U),
                rootValue = raw.booleanArrayOf(rootValue, C, K, U),
                childValueLeft = raw.booleanArrayOf(childValueLeft, C, K, P, U),
                childValueRight = raw.booleanArrayOf(childValueRight, C, K, P, U),
                firstFired = raw.intArrayOf(firstFired, C, U, domain = 1..K) { 0 },
                notFired = raw.booleanArrayOf(notFired, C, U, K)
            )
        }
    }
}

@Suppress("LocalVariableName")
internal fun ExtendedAssignment.toAutomaton(): Automaton {
    val automaton = Automaton(scenarioTree)

    for (c in 1..C)
        automaton.addState(
            id = c,
            outputEvent = scenarioTree.outputEvents[outputEvent[c] - 1],
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
                        childLeft = IntMultiArray.create(P) { (p) -> childLeft[c, k, p] },
                        childRight = IntMultiArray.create(P) { (p) -> childRight[c, k, p] },
                        inputNames = scenarioTree.inputNames
                    )
                )

    return automaton
}
