package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.VarEncoding
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newIntVarArray
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.pow

@Suppress("PropertyName")
class ArbitraryModularBasicVariables(
    val scenarioTree: PositiveScenarioTree,
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
    /* Cardinality */
    val cardinality: Cardinality,
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
    scenarioTree: PositiveScenarioTree,
    M: Int,
    C: Int,
    K: Int,
    V: Int = scenarioTree.size,
    E: Int = scenarioTree.inputEvents.size,
    O: Int = scenarioTree.outputEvents.size,
    X: Int = scenarioTree.inputNames.size,
    Z: Int = scenarioTree.outputNames.size
) {
    check(E == 1 && O == 1) { "Only one input/output event is supported" }

    val U = 2.pow(X)
    log.debug { "U = $U" }
    log.debug { "V = $V" }

    comment("ArbitraryModularBasicVariables")

    /* Core variables */
    comment("Core variables")
    val modularActualTransitionFunction by context(MultiArray.create(M) { newIntVarArray(C, U) { 0..C } })
    val modularTransitionDestination by context(MultiArray.create(M) { newIntVarArray(C, K) { 0..C } })
    val modularTransitionFiring by context(MultiArray.create(M) { newBoolVarArray(C, K, U) })
    val modularFirstFired by context(MultiArray.create(M) { newIntVarArray(C, U) { 0..K } })
    val modularNotFired by context(MultiArray.create(M) { newBoolVarArray(C, K, U) })
    // val modularStateOutputEvent by context(MultiArray.create(M) { newIntVar(C) { 0..1 } })
    val modularStateAlgorithmTop by context(MultiArray.create(M) { newBoolVarArray(C, Z) })
    val modularStateAlgorithmBot by context(MultiArray.create(M) { newBoolVarArray(C, Z) })

    /* Mapping variables */
    comment("Mapping variables")
    val modularMapping by context(MultiArray.create(M) { newIntVarArray(V) { 1..C } })

    /* Cardinality */
    comment("Cardinality (T)")
    val cardinalityT by context(declareCardinality {
        for (m in 1..M) {
            for (c in 1..C)
                for (k in 1..K)
                    yield(modularTransitionDestination[m][c, k] neq 0)
        }
    })

    /* Modular variables */
    comment("Modular variables")
    with(Pins(M, X, Z, E, O)) {
        val inboundVarPinParent by context(newIntVarArray(allInboundVarPins.size) { (p) ->
            if (p in externalInboundVarPins) {
                (0..allOutboundVarPins.size)
            } else {
                val m = (p - 1) / X + 1
                (1 until m).flatMap { m_ -> modularOutboundVarPins[m_] } + externalOutboundVarPins + 0
            }
        })
        val inboundEventPinParent by context(newIntVarArray(allInboundEventPins.size) { (p) ->
            if (p in externalInboundVarPins) {
                listOf(modularOutboundEventPins[M][0])
            } else {
                val m = (p - 1) / E + 1
                if (m > 1)
                    listOf(m - 1)
                else
                    listOf(externalInboundEventPins[0])
            }
        })
        val modularInputIndex by context(MultiArray.create(M) {
            newIntVarArray(V, encoding = VarEncoding.ONEHOT_BINARY) { 1..U }
        })
        val inboundVarPinComputedValue by context(newBoolVarArray(V, allInboundVarPins.size) { (v, p) ->
            if (p in externalInboundVarPins) {
                newLiteral()
            } else {
                val m = (p - 1) / X + 1
                val x = p - (m - 1) * X
                modularInputIndex[m][v].bit(x - 1)
            }
        })
        val outboundVarPinComputedValue by context(newBoolVarArray(V, allOutboundVarPins.size))
        val inboundEventPinConsumedEvent by context(newIntVarArray(V, allInboundEventPins.size) { listOf(0, 1) })
        val outboundEventPinEmittedEvent by context(newIntVarArray(V, allOutboundEventPins.size) { listOf(0, 1) })
    }
}
