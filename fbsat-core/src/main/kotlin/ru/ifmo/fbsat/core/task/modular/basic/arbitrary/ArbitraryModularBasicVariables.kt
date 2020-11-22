package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.VarEncoding
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.declareModularContext
import ru.ifmo.fbsat.core.solver.forEachModularContext
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newIntVarArray
import ru.ifmo.fbsat.core.task.single.basic.declareBasicVariables
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.pow

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
    Z: Int = scenarioTree.outputNames.size,
) {
    check(E == 1 && O == 1) { "Only one input/output event is supported" }

    val U = 2.pow(X)
    log.debug { "U = $U" }
    log.debug { "V = $V" }

    context["positiveScenarioTree"] = scenarioTree
    context["scenarioTree"] = scenarioTree
    context["tree"] = scenarioTree
    context["M"] = M
    context["C"] = C
    context["K"] = K
    context["V"] = V
    context["E"] = E
    context["O"] = O
    context["X"] = X
    context["Z"] = Z
    context["U"] = U

    /* Modular */
    declareModularContext(M)
    forEachModularContext { m ->
        comment("BasicVariables for module m = $m")
        declareBasicVariables(
            positiveScenarioTree = scenarioTree,
            C = C, K = K,
            V = V, E = E, O = O, X = X, Z = Z, U = U
        )
    }

    /* Cardinality */
    comment("Cardinality (T)")
    val cardinalityT = context("cardinalityT") {
        declareCardinality {
            forEachModularContext {
                val transitionDestination: IntVarArray = context["transitionDestination"]
                for (c in 1..C)
                    for (k in 1..K)
                        yield(transitionDestination[c, k] neq 0)
            }
        }
    }

    /* Modular variables */
    comment("Modular variables")
    with(Pins(M = M, X = X, Z = Z, E = E, O = O)) {
        val inboundVarPinParent = context("inboundVarPinParent") {
            newIntVarArray(allInboundVarPins.size) { (p) ->
                if (p in externalInboundVarPins) {
                    (0..allOutboundVarPins.size)
                } else {
                    val m = (p - 1) / X + 1
                    (1 until m).flatMap { m_ -> modularOutboundVarPins[m_] } + externalOutboundVarPins + 0
                }
            }
        }
        val inboundEventPinParent = context("inboundEventPinParent") {
            newIntVarArray(allInboundEventPins.size) { (p) ->
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
        }
        val modularInputIndex = context("modularInputIndex") {
            MultiArray.create(M) {
                newIntVarArray(V, encoding = VarEncoding.ONEHOT_BINARY) { 1..U }
            }
        }
        val inboundVarPinComputedValue = context("inboundVarPinComputedValue") {
            newBoolVarArray(V, allInboundVarPins.size) { (v, p) ->
                if (p in externalInboundVarPins) {
                    newLiteral()
                } else {
                    val m = (p - 1) / X + 1
                    val x = p - (m - 1) * X
                    modularInputIndex[m][v].bit(x - 1)
                }
            }
        }
        val outboundVarPinComputedValue = context("outboundVarPinComputedValue") {
            newBoolVarArray(V, allOutboundVarPins.size)
        }
        val inboundEventPinConsumedEvent = context("inboundEventPinConsumedEvent") {
            newIntVarArray(V, allInboundEventPins.size) { listOf(0, 1) }
        }
        val outboundEventPinEmittedEvent = context("outboundEventPinEmittedEvent") {
            newIntVarArray(V, allOutboundEventPins.size) { listOf(0, 1) }
        }
    }
}
