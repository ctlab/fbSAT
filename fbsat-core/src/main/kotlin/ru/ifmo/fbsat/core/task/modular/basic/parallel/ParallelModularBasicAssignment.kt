package ru.ifmo.fbsat.core.task.modular.basic.parallel

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.ParallelModularAutomaton
import ru.ifmo.fbsat.core.automaton.TruthTableGuard
import ru.ifmo.fbsat.core.automaton.endow
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.solver.convert
import ru.ifmo.fbsat.core.task.single.basic.BasicAssignment

class ParallelModularBasicAssignment(
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
    val modularBasicAssignment: MultiArray<BasicAssignment>,
    /* Interface variables */
    val moduleControllingOutputVariable: IntMultiArray // [Z] : 1..M
) {
    @Suppress("PropertyName")
    val T: Int = (1..M).sumBy { m -> modularBasicAssignment[m].transitionDestination.values.count { it != 0 } }

    /** `[M] : {1..Z}` */
    val moduleOutputVariables: MultiArray<List<Int>> =
        MultiArray.create(M) { (m) ->
            (1..Z).filter { z -> moduleControllingOutputVariable[z] == m }
        }

    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(
            raw: RawAssignment,
            vars: ParallelModularBasicVariables
        ): ParallelModularBasicAssignment = with(vars) {
            ParallelModularBasicAssignment(
                scenarioTree = scenarioTree,
                M = M, C = C, K = K,
                V = V, E = E, O = O, X = X, Z = Z, U = U,
                modularBasicAssignment = MultiArray.create(M) { (m) ->
                    BasicAssignment.fromRaw(raw, modularBasicVariables[m])
                },
                moduleControllingOutputVariable = moduleControllingOutputVariable.convert(raw)
            )
        }
    }
}

fun ParallelModularBasicAssignment.toAutomaton(): ParallelModularAutomaton {
    val modules = MultiArray.create(M) { (m) ->
        with(modularBasicAssignment[m]) {
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
                    BinaryAlgorithm(
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
                    TruthTableGuard(
                        truthTable = (1..scenarioTree.uniqueInputs.size)
                            .asSequence()
                            .associate { u ->
                                scenarioTree.uniqueInputs[u - 1] to
                                    when {
                                        notFired[c, k, u] -> false
                                        firstFired[c, u] == k -> true
                                        else -> null
                                    }
                            },
                        inputNames = scenarioTree.inputNames,
                        uniqueInputs = scenarioTree.uniqueInputs
                    )
                }
            )
        }
    }

    return ParallelModularAutomaton(modules, moduleControllingOutputVariable)
}
