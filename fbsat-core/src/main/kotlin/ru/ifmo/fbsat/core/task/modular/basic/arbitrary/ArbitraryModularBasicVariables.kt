package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.VarEncoding
import ru.ifmo.fbsat.core.utils.log
import kotlin.math.pow

@Suppress("PropertyName")
class ArbitraryModularBasicVariables(
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
    val modularActualTransitionFunction: MultiArray<IntVarArray>,
    val modularTransitionDestination: MultiArray<IntVarArray>,
    val modularTransitionFiring: MultiArray<BoolVarArray>,
    val modularFirstFired: MultiArray<IntVarArray>,
    val modularNotFired: MultiArray<BoolVarArray>,
    val modularStateAlgorithmTop: MultiArray<BoolVarArray>,
    val modularStateAlgorithmBot: MultiArray<BoolVarArray>,
    /* Mapping variables */
    val modularMapping: MultiArray<IntVarArray>,
    /* Modular variables */
    val inboundVarPinParent: IntVarArray,
    val inboundEventPinParent: IntVarArray,
    val modularInputIndex: MultiArray<IntVarArray>,
    val inboundVarPinComputedValue: BoolVarArray,
    val outboundVarPinComputedValue: BoolVarArray,
    val inboundEventPinConsumedEvent: IntVarArray, // {0,1}=={epsilon, REQ/CNF}
    val outboundEventPinEmittedEvent: IntVarArray // {0,1}=={epsilon, REQ/CNF}
)

@Suppress("LocalVariableName")
fun Solver.declareArbitraryModularBasicVariables(
    scenarioTree: ScenarioTree,
    M: Int,
    C: Int,
    K: Int,
    V: Int = scenarioTree.size,
    E: Int = scenarioTree.inputEvents.size,
    O: Int = scenarioTree.outputEvents.size,
    X: Int = scenarioTree.inputNames.size,
    Z: Int = scenarioTree.outputNames.size
): ArbitraryModularBasicVariables {
    check(E == 1 && O == 1) { "Only one input/output event is supported" }

    val U = 2.toDouble().pow(X).toInt()
    log.debug { "U = $U" }
    log.debug { "V = $V" }

    comment("ArbitraryModularBasicVariables")
    /* Core variables */
    val modularActualTransitionFunction = MultiArray.create(M) { newIntVarArray(C, U) { 0..C } }
    val modularTransitionDestination = MultiArray.create(M) { newIntVarArray(C, K) { 0..C } }
    val modularTransitionFiring = MultiArray.create(M) { newBoolVarArray(C, K, U) }
    val modularFirstFired = MultiArray.create(M) { newIntVarArray(C, U) { 0..K } }
    val modularNotFired = MultiArray.create(M) { newBoolVarArray(C, K, U) }
    // val modularStateOutputEvent = MultiArray.create(M) { newIntVar(C) { 0..1 } }
    val modularStateAlgorithmTop = MultiArray.create(M) { newBoolVarArray(C, Z) }
    val modularStateAlgorithmBot = MultiArray.create(M) { newBoolVarArray(C, Z) }
    /* Mapping variables */
    val modularMapping = MultiArray.create(M) { newIntVarArray(V) { 1..C } }
    /* Modular variables */
    with(PinVars(M, X, Z, E, O)) {
        val inboundVarPinParent = newIntVarArray(allInboundVarPins.size) { (p) ->
            if (p in externalInboundVarPins) {
                (0..allOutboundVarPins.size)
            } else {
                val m = (p - 1) / X + 1
                (1 until m).flatMap { m_ -> modularOutboundVarPins[m_] } + externalOutboundVarPins + 0
            }
        }
        val inboundEventPinParent = newIntVarArray(allInboundEventPins.size) { (p) ->
            if (p in externalInboundVarPins) {
                listOf(modularOutboundEventPins[M][0])
            } else {
                val m = (p - 1) / E + 1
                if (m > 1)
                    listOf(m - 1)
                else
                    listOf(externalInboundEventPins[0])
            }
        }
        val modularInputIndex = MultiArray.create(M) {
            newIntVarArray(V, encoding = VarEncoding.ONEHOT_BINARY) { 1..U }
        }
        // val inboundVarPinComputedValue = newBoolVarArray(V, allInboundVarPins.size)
        val inboundVarPinComputedValue = newBoolVarArray(V, allInboundVarPins.size) { (v, p) ->
            if (p in externalInboundVarPins) {
                newLiteral()
            } else {
                val m = (p - 1) / X + 1
                val x = p - (m - 1) * X
                modularInputIndex[m][v].bit(x - 1)
            }
        }
        val outboundVarPinComputedValue = newBoolVarArray(V, allOutboundVarPins.size)
        val inboundEventPinConsumedEvent = newIntVarArray(V, allInboundEventPins.size) { listOf(0, 1) }
        val outboundEventPinEmittedEvent = newIntVarArray(V, allOutboundEventPins.size) { listOf(0, 1) }

        return ArbitraryModularBasicVariables(
            scenarioTree = scenarioTree,
            M = M, C = C, K = K,
            V = V, E = E, O = O, X = X, Z = Z, U = U,
            modularActualTransitionFunction = modularActualTransitionFunction,
            modularTransitionDestination = modularTransitionDestination,
            modularTransitionFiring = modularTransitionFiring,
            modularFirstFired = modularFirstFired,
            modularNotFired = modularNotFired,
            modularStateAlgorithmTop = modularStateAlgorithmTop,
            modularStateAlgorithmBot = modularStateAlgorithmBot,
            modularMapping = modularMapping,
            inboundVarPinParent = inboundVarPinParent,
            inboundEventPinParent = inboundEventPinParent,
            modularInputIndex = modularInputIndex,
            inboundVarPinComputedValue = inboundVarPinComputedValue,
            outboundVarPinComputedValue = outboundVarPinComputedValue,
            inboundEventPinConsumedEvent = inboundEventPinConsumedEvent,
            outboundEventPinEmittedEvent = outboundEventPinEmittedEvent
        )
    }
}
