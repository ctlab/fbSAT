package ru.ifmo.fbsat.core.task.single.basic2

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.ECC
import ru.ifmo.fbsat.core.automaton.TruthTableGuard
import ru.ifmo.fbsat.core.automaton.endow
import ru.ifmo.fbsat.core.scenario2.positive.ScenarioTree2
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.solver.convert
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.countTrue
import ru.ifmo.fbsat.core.utils.withIndex

class BasicAssignment2(
    val scenarioTree: ScenarioTree2,
    /* Constants */
    val C: Int,
    val K: Int,
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
    val transitionTruthTable: BooleanMultiArray, // [C, K, E, U] : Boolean
    val transitionFiring: BooleanMultiArray, // [C, K, E, U] : Boolean
    val firstFired: IntMultiArray, // [C, E, U] : 0..K
    val notFired: BooleanMultiArray, // [C, K, E, U] : Boolean
    val stateOutputEvent: IntMultiArray, // [C] : 0..O
    val stateAlgorithmTop: BooleanMultiArray, // [C, Z] : Boolean
    val stateAlgorithmBot: BooleanMultiArray, // [C, Z] : Boolean
    /* Mapping variables */
    val mapping: IntMultiArray // [V] : 1..C
) {
    /**
     * Number of transitions.
     */
    @Suppress("PropertyName")
    val T: Int = transitionDestination.values.count { it != 0 }

    companion object {
        fun fromRaw(
            raw: RawAssignment,
            vars: BasicVariables2
        ): BasicAssignment2 = with(vars) {
            BasicAssignment2(
                scenarioTree = scenarioTree,
                C = C, K = K,
                V = V, E = E, O = O, X = X, Z = Z, U = U,
                actualTransitionFunction = actualTransitionFunction.convert(raw),
                transitionDestination = transitionDestination.convert(raw),
                transitionInputEvent = transitionInputEvent.convert(raw),
                transitionTruthTable = transitionTruthTable.convert(raw),
                transitionFiring = transitionFiring.convert(raw),
                firstFired = firstFired.convert(raw),
                notFired = notFired.convert(raw),
                stateOutputEvent = stateOutputEvent.convert(raw),
                stateAlgorithmTop = stateAlgorithmTop.convert(raw),
                stateAlgorithmBot = stateAlgorithmBot.convert(raw),
                mapping = mapping.convert(raw)
            ).also {
                check(it.T == cardinality.totalizer.convert(raw).values.countTrue())
            }
        }
    }
}

fun BasicAssignment2.toAutomaton(): ECC =
    ECC(scenarioTree).endow(
        C = C, K = K,
        stateActions = { c: Int ->
            sequence {
                for (a in 1..Globals.OUTPUT_ACTIONS_MULTIPLICITY) {
                    val o = stateOutputEvent[c, a]
                    if (o != 0) {
                        yield(ECC.Action(
                            scenarioTree.outputEvents[o - 1],
                            BinaryAlgorithm(
                                algorithm0 = BooleanArray(Z) { z0 -> stateAlgorithmBot[c, a, z0 + 1] },
                                algorithm1 = BooleanArray(Z) { z0 -> stateAlgorithmTop[c, a, z0 + 1] }
                            )
                        ))
                    }
                }
            }.toList()
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
