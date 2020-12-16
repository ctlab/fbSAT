package ru.ifmo.fbsat.core.task.single.basic

import com.github.lipen.satlib.card.declareCardinality
import com.github.lipen.satlib.core.newBoolVarArray
import com.github.lipen.satlib.core.newIntVarArray
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.literals
import ru.ifmo.fbsat.core.utils.Globals

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

    /* Core variables */
    comment("Core variables")
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
        newIntVarArray(C, E, U) { 0..K }
    }
    val notFired = context("notFired") {
        newBoolVarArray(C, K, E, U)
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

    /* Mapping variables */
    comment("Mapping variables")
    val mapping = context("mapping") {
        newIntVarArray(V) { 1..C }
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

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        comment("actualTransitionFunction = ${actualTransitionFunction.literals}")
        comment("transitionDestination = ${transitionDestination.literals}")
        comment("transitionInputEvent = ${transitionInputEvent.literals}")
        comment("transitionTruthTable = ${transitionTruthTable.literals}")
        comment("transitionFiring = ${transitionFiring.literals}")
        comment("firstFired = ${firstFired.literals}")
        comment("notFired = ${notFired.literals}")
        comment("stateOutputEvent = ${stateOutputEvent.literals}")
        comment("stateAlgorithmBot = ${stateAlgorithmBot.literals}")
        comment("stateAlgorithmTop = ${stateAlgorithmTop.literals}")
        comment("mapping = ${mapping.literals}")
        comment("totalizerT = ${cardinalityT.totalizer}")
    }
}
