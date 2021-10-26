package ru.ifmo.fbsat.core.task.single.basic

import com.github.lipen.satlib.card.declareCardinality
import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.eq
import com.github.lipen.satlib.core.neq
import com.github.lipen.satlib.core.newBoolVarArray
import com.github.lipen.satlib.core.newIntVar
import com.github.lipen.satlib.core.newIntVarArray
import com.github.lipen.satlib.op.*
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.scenario.initialOutputValues
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger

private val logger = MyLogger {}

@Suppress("LocalVariableName")
fun Solver.declareBasicVariables(
    positiveScenarioTree: PositiveScenarioTree,
    C: Int,
    K: Int,
    V: Int = positiveScenarioTree.size,
    E: Int = positiveScenarioTree.inputEvents.size,
    O: Int = positiveScenarioTree.outputEvents.size,
    X: Int = positiveScenarioTree.inputNames.size,
    Z: Int = positiveScenarioTree.outputNames.size,
    U: Int = positiveScenarioTree.uniqueInputs.size,
) {
    context["positiveScenarioTree"] = positiveScenarioTree
    context["scenarioTree"] = positiveScenarioTree
    context["positiveTree"] = positiveScenarioTree
    context["tree"] = positiveScenarioTree
    context["C"] = C
    context["K"] = K
    context["V"] = V
    context["E"] = E
    context["O"] = O
    context["X"] = X
    context["Z"] = Z
    context["U"] = U
    context["initialOutputValues"] = positiveScenarioTree.initialOutputValues

    /* Core variables */
    comment("Core variables")
    if (Globals.IS_ENCODE_EVENTLESS) {
        if (Globals.IS_ENCODE_TRANSITION_FUNCTION) {
            val transitionFunction = context("transitionFunction") {
                newIntVarArray(C, E, U) { 1..C }
            }
        }
    }
    val actualTransitionFunction = context("actualTransitionFunction") {
        newIntVarArray(C, E, U) { 0..C }
    }
    val transitionDestination = context("transitionDestination") {
        newIntVarArray(C, K) { 0..C }
    }
    val transitionInputEvent = context("transitionInputEvent") {
        newIntVarArray(C, K) { 0..E }
    }
    val transitionTruthTable = context("transitionTruthTable") {
        newBoolVarArray(C, K, U)
    }
    val transitionFiring = context("transitionFiring") {
        newBoolVarArray(C, K, E, U)
    }
    val firstFired = context("firstFired") {
        if (Globals.IS_ENCODE_FF_0_VARDECL) {
            IntVarArray.new(C, E, U) { (c, e, u) ->
                newIntVar(0..K) { k ->
                    if (k == 0)
                        actualTransitionFunction[c, e, u] eq 0
                    else
                        newLiteral()
                }
            }
        } else {
            newIntVarArray(C, E, U) { 0..K }
        }
    }
    val notFired = context("notFired") {
        if (Globals.IS_ENCODE_FF_NF_VARDECL) {
            newBoolVarArray(C, K, E, U) { (c, k, e, u) ->
                if (k == K)
                    firstFired[c, e, u] eq 0
                else
                    newLiteral()
            }
        } else {
            newBoolVarArray(C, K, E, U)
        }
    }
    val stateOutputEvent = context("stateOutputEvent") {
        newIntVarArray(C) { 0..O }
    }
    val stateAlgorithmBot = context("stateAlgorithmBot") {
        newBoolVarArray(C, Z)
    }
    val stateAlgorithmTop = context("stateAlgorithmTop") {
        newBoolVarArray(C, Z)
    }
    if (Globals.IS_ENCODE_CONJUNCTIVE_GUARDS) {
        val inputVariableUsed = context("inputVariableUsed") {
            newBoolVarArray(X)
        }
        val inputVariableLiteral = context("inputVariableLiteral") {
            newBoolVarArray(C, K, X)
        }
    }

    /* Mapping variables */
    comment("Mapping variables")
    val mapping = context("mapping") {
        newIntVarArray(V) { 1..C }
    }
    if (Globals.IS_ENCODE_EVENTLESS) {
        if (!context.map.containsKey("active")) {
            val active = context("active") {
                newBoolVarArray(V)
            }
        }
    }

    /* Cardinality */
    comment("Cardinality (T)")
    val cardinalityT = context("cardinalityT") {
        declareCardinality {
            for (c in 1..C)
                for (k in 1..K)
                    yield(transitionDestination[c, k] neq 0)
        }
    }
    if (Globals.IS_ENCODE_CONJUNCTIVE_GUARDS) {
        comment("Cardinality (T*A)")
        val inputVariableUsed: BoolVarArray = context["inputVariableUsed"]
        val cardinalityTA = context("cardinalityTA") {
            val nVars = numberOfVariables
            val nCons = numberOfClauses
            declareCardinality {
                for (c in 1..C)
                    for (k in 1..K)
                        for (x in 1..X) {
                            val aux = newLiteral()
                            iffAnd(
                                aux,
                                transitionDestination[c, k] neq 0,
                                inputVariableUsed[x]
                            )
                            yield(aux)
                        }
            }.also {
                val diffVars = numberOfVariables - nVars
                val diffCons = numberOfClauses - nCons
                logger.debug("Declared cardinalityTA: $diffVars vars and $diffCons clauses")
            }
        }
    }
}
