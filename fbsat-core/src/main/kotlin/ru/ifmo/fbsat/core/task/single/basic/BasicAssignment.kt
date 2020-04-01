package ru.ifmo.fbsat.core.task.single.basic

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.TruthTableGuard
import ru.ifmo.fbsat.core.automaton.endowed
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.solver.convert
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.withIndex

class BasicAssignment(
    val scenarioTree: ScenarioTree,
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
    val transitionFiring: BooleanMultiArray, // [C, K, U] : Boolean
    val firstFired: IntMultiArray, // [C, U] : 0..K
    val notFired: BooleanMultiArray, // [C, K, U] : Boolean
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
            vars: BasicVariables
        ): BasicAssignment = with(vars) {
            BasicAssignment(
                scenarioTree = scenarioTree,
                C = C, K = K,
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
                mapping = mapping.convert(raw)
            ).also {
                check(it.T == cardinality.totalizer.convert(raw).values.count { b -> b })
            }
        }
    }
}

fun BasicAssignment.toAutomaton(): Automaton =
    Automaton(scenarioTree).endowed(
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
                truthTable = scenarioTree.uniqueInputs.withIndex(start = 1)
                    .associate { (u, input) ->
                        input to
                            if (Globals.IS_PARTIAL_TRUTH_TABLES)
                                when {
                                    notFired[c, k, u] -> false
                                    firstFired[c, u] == k -> true
                                    else -> null
                                }
                            else
                                transitionFiring[c, k, u]
                    },
                inputNames = scenarioTree.inputNames,
                uniqueInputs = scenarioTree.uniqueInputs
            )
        }
    )
