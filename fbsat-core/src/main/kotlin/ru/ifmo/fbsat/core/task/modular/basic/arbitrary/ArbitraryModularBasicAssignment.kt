package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.ArbitraryModularAutomaton
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.automaton.TruthTableGuard
import ru.ifmo.fbsat.core.automaton.endowed
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.solver.convert
import ru.ifmo.fbsat.core.utils.log

class ArbitraryModularBasicAssignment(
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
    /* Core variables */
    val modularActualTransitionFunction: MultiArray<IntMultiArray>,
    val modularTransitionDestination: MultiArray<IntMultiArray>,
    val modularTransitionFiring: MultiArray<BooleanMultiArray>,
    val modularFirstFired: MultiArray<IntMultiArray>,
    val modularNotFired: MultiArray<BooleanMultiArray>,
    val modularStateAlgorithmTop: MultiArray<BooleanMultiArray>,
    val modularStateAlgorithmBot: MultiArray<BooleanMultiArray>,
    /* Mapping variables */
    val modularMapping: MultiArray<IntMultiArray>,
    /* Modular variables */
    val inboundVarPinParent: IntMultiArray, // [vIP]: 0..vIP
    val inboundEventPinParent: IntMultiArray, // [eIP]: 1..eIP
    val inboundVarPinComputedValue: BooleanMultiArray,
    val outboundVarPinComputedValue: BooleanMultiArray,
    val modularInputIndex: MultiArray<IntMultiArray>
) {
    /**
     * Number of transitions.
     */
    @Suppress("PropertyName")
    val T: Int = (1..M).sumBy { m -> modularTransitionDestination[m].count { it != 0 } }

    init {
        for (m in 1..M) {
            log.success("modularInputIndex[m = $m] = ${(1..V).map { v -> modularInputIndex[m][v] }}")
        }
        with(PinVars(M, X, Z, E, O)) {
            for (m in 1..M) {
                log.success("inboundVarPinParent for module m = $m: ${modularInboundVarPins[m].map { pin -> inboundVarPinParent[pin] }}")
            }
            log.success("inboundVarPinParent for external output variables: ${externalInboundVarPins.map { pin -> inboundVarPinParent[pin] }}")

            log.success("input variables connections: " + (1..X).joinToString(", ") { x -> "x$x:${allInboundVarPins.filter { inboundVarPinParent[it] == externalOutboundVarPins[x - 1] }}" })
            for (m in 1..M) {
                log.success("output variables connections for module m = $m: " + (1..Z).joinToString(", ") { z -> "z$z:${allInboundVarPins.filter { inboundVarPinParent[it] == modularOutboundVarPins[m][z - 1] }}" })
            }
        }
    }

    companion object {
        fun fromRaw(
            raw: RawAssignment,
            vars: ArbitraryModularBasicVariables
        ): ArbitraryModularBasicAssignment = with(vars) {
            ArbitraryModularBasicAssignment(
                scenarioTree = scenarioTree,
                M = M, C = C, K = K,
                V = V, E = E, O = O, X = X, Z = Z, U = U,
                modularActualTransitionFunction = modularActualTransitionFunction.convert(raw),
                modularTransitionDestination = modularTransitionDestination.convert(raw),
                modularTransitionFiring = modularTransitionFiring.convert(raw),
                modularFirstFired = modularFirstFired.convert(raw),
                modularNotFired = modularNotFired.convert(raw),
                modularStateAlgorithmTop = modularStateAlgorithmTop.convert(raw),
                modularStateAlgorithmBot = modularStateAlgorithmBot.convert(raw),
                modularMapping = modularMapping.convert(raw),
                inboundVarPinParent = inboundVarPinParent.convert(raw),
                inboundEventPinParent = inboundEventPinParent.convert(raw),
                inboundVarPinComputedValue = inboundVarPinComputedValue.convert(raw),
                outboundVarPinComputedValue = outboundVarPinComputedValue.convert(raw),
                modularInputIndex = modularInputIndex.convert(raw)
            )
        }
    }
}

@Suppress("LocalVariableName")
fun ArbitraryModularBasicAssignment.toAutomaton(): ArbitraryModularAutomaton {
    val modules = MultiArray.create(M) { (m) ->
        Automaton(scenarioTree).endowed(
            C = C, K = K,
            stateOutputEvent = { OutputEvent("CNF") },
            stateAlgorithm = { c ->
                BinaryAlgorithm(
                    algorithm0 = BooleanArray(Z) { z0 -> modularStateAlgorithmBot[m][c, z0 + 1] },
                    algorithm1 = BooleanArray(Z) { z0 -> modularStateAlgorithmTop[m][c, z0 + 1] }
                )
            },
            transitionDestination = { c, k -> modularTransitionDestination[m][c, k] },
            transitionInputEvent = { _, _ -> InputEvent("REQ") },
            transitionGuard = { c, k ->
                TruthTableGuard(
                    truthTable = (1..scenarioTree.uniqueInputs.size)
                        .asSequence()
                        .associate { u ->
                            scenarioTree.uniqueInputs[u - 1] to
                                when {
                                    modularNotFired[m][c, k, u] -> false
                                    modularFirstFired[m][c, u] == k -> true
                                    else -> null
                                }
                        },
                    inputNames = scenarioTree.inputNames,
                    uniqueInputs = scenarioTree.uniqueInputs
                )
            }

        )
    }

    return ArbitraryModularAutomaton(modules, inboundVarPinParent, scenarioTree)
}
