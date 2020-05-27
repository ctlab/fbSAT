package ru.ifmo.fbsat.core.task.modular.extended.parallel

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.automaton.ParallelModularAutomaton
import ru.ifmo.fbsat.core.automaton.ParseTreeGuard
import ru.ifmo.fbsat.core.automaton.endow
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.solver.convert
import ru.ifmo.fbsat.core.task.single.extended.ExtendedAssignment
import ru.ifmo.fbsat.core.utils.withIndex

@Suppress("PropertyName")
class ParallelModularExtendedAssignment(
    val scenarioTree: ScenarioTree,
    val M: Int,
    val C: Int,
    val K: Int,
    val P: Int,
    val V: Int,
    val E: Int,
    val O: Int,
    val X: Int,
    val Z: Int,
    val U: Int,
    /* Modularized ExtendedAssignment */
    val modularExtendedAssignment: MultiArray<ExtendedAssignment>,
    /* Interface variables */
    val moduleControllingOutputVariable: IntMultiArray
) {
    /**
     * Number of transitions.
     */
    @Suppress("PropertyName")
    val T: Int = modularExtendedAssignment.values.sumBy { it.T }

    /**
     * Total guards size (total number of nodes in all parse trees).
     */
    @Suppress("PropertyName")
    val N: Int = modularExtendedAssignment.values.sumBy { it.N }

    /** `[M] : {1..Z}` */
    val moduleOutputVariables: MultiArray<List<Int>> =
        MultiArray.create(M) { (m) ->
            (1..Z).filter { z -> moduleControllingOutputVariable[z] == m }
        }

    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(
            raw: RawAssignment,
            vars: ParallelModularExtendedVariables
        ): ParallelModularExtendedAssignment = with(vars) {
            ParallelModularExtendedAssignment(
                scenarioTree = scenarioTree,
                M = M, C = C, K = K, P = P,
                V = V, E = E, O = O, X = X, Z = Z, U = U,
                modularExtendedAssignment = MultiArray.create(M) { (m) ->
                    ExtendedAssignment.fromRaw(raw, modularExtendedVariables[m])
                },
                moduleControllingOutputVariable = moduleControllingOutputVariable.convert(raw)
            )
        }
    }
}

@Suppress("LocalVariableName")
fun ParallelModularExtendedAssignment.toAutomaton(): ParallelModularAutomaton {
    val modules = MultiArray.create(M) { (m) ->
        with(modularExtendedAssignment[m]) {
            Automaton(
                scenarioTree.inputEvents,
                scenarioTree.outputEvents,
                scenarioTree.inputNames,
                moduleOutputVariables[m].map { z -> scenarioTree.outputNames[z - 1] }
            ).endow(
                C = C, K = K,
                stateOutputEvent = { c ->
                    stateOutputEvent[c].let { o ->
                        if (o == 0) null else scenarioTree.outputEvents[o - 1]
                    }
                },
                stateAlgorithm = { c ->
                    ru.ifmo.fbsat.core.automaton.BinaryAlgorithm(
                        algorithm0 = moduleOutputVariables[m].map { z -> stateAlgorithmBot[c, z] },
                        algorithm1 = moduleOutputVariables[m].map { z -> stateAlgorithmTop[c, z] }
                    )
                },
                transitionDestination = { c, k ->
                    transitionDestination[c, k]
                },
                transitionInputEvent = { c, k ->
                    scenarioTree.inputEvents[transitionInputEvent[c, k] - 1]
                },
                transitionGuard = { c, k ->
                    ParseTreeGuard(
                        nodeType = MultiArray.create(P) { (p) -> nodeType[c, k, p] },
                        terminal = IntMultiArray.create(P) { (p) -> nodeInputVariable[c, k, p] },
                        parent = IntMultiArray.create(P) { (p) -> nodeParent[c, k, p] },
                        childLeft = IntMultiArray.create(P) { (p) -> nodeChild[c, k, p] },
                        childRight = IntMultiArray.create(P) { (p) ->
                            if (nodeType[c, k, p] in setOf(NodeType.AND, NodeType.OR))
                                nodeChild[c, k, p] + 1
                            else
                                0
                        },
                        inputNames = scenarioTree.inputNames
                    )
                }
            )
        }
    }

    return ParallelModularAutomaton(modules, moduleControllingOutputVariable)
}
