package ru.ifmo.fbsat.core.task.distributed

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.automaton.TruthTableGuard
import ru.ifmo.fbsat.core.automaton.endow
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.task.single.basic.BasicAssignment
import ru.ifmo.fbsat.core.utils.withIndex

class DistributedBasicAssignment(
    val scenarioTree: ScenarioTree,
    /* Constants */
    val M: Int,
    val C: Int,
    val K: Int,
    val V: Int,
    val E: Int,
    val O: Int,
    val X: Int,
    val Z: Int,
    val U: Int,
    /* Modularized BasicAssignment */
    val modularBasicAssignment: MultiArray<BasicAssignment>
) {
    @Suppress("PropertyName")
    val T: Int = (1..M).sumBy { m -> modularBasicAssignment[m].transitionDestination.values.count { it != 0 } }

    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(
            raw: RawAssignment,
            vars: DistributedBasicVariables
        ): DistributedBasicAssignment = with(vars) {
            DistributedBasicAssignment(
                scenarioTree = scenarioTree,
                M = M, C = C, K = K,
                V = V, E = E, O = O, X = X, Z = Z, U = U,
                modularBasicAssignment = MultiArray.create(M) { (m) ->
                    BasicAssignment.fromRaw(raw, modularBasicVariables[m])
                }
            )
        }
    }
}

fun DistributedBasicAssignment.toAutomaton(): DistributedAutomaton {
    val modules = MultiArray.create(M) { (m) ->
        with(modularBasicAssignment[m]) {
            Automaton(
                scenarioTree.inputEvents,
                scenarioTree.outputEvents,
                scenarioTree.inputNames,
                scenarioTree.outputNames
            ).endow(
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
                    TruthTableGuard(
                        truthTable = scenarioTree.uniqueInputs
                            .withIndex(start = 1)
                            .associate { (u, input) ->
                                input to transitionTruthTable[c, k, u]
                            },
                        inputNames = scenarioTree.inputNames,
                        uniqueInputs = scenarioTree.uniqueInputs
                    )
                }
            )
        }
    }

    return DistributedAutomaton(modules)
}
