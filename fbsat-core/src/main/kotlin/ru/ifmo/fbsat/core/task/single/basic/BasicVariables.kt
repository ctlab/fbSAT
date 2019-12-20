package ru.ifmo.fbsat.core.task.single.basic

import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.BoolVar
import ru.ifmo.fbsat.core.solver.IntVar
import ru.ifmo.fbsat.core.solver.Solver

@Suppress("PropertyName")
class BasicVariables(
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
    val transitionDestination: IntVar,
    val transitionInputEvent: IntVar,
    val transitionFiring: BoolVar,
    val firstFired: IntVar,
    val notFired: BoolVar,
    val stateOutputEvent: IntVar,
    val stateAlgorithmTop: BoolVar,
    val stateAlgorithmBot: BoolVar,
    /* Interface variables */
    val actualTransitionFunction: IntVar,
    /* Mapping variables */
    val mapping: IntVar
)

fun Solver.declareBasicVariables(
    scenarioTree: ScenarioTree,
    C: Int,
    K: Int,
    V: Int = scenarioTree.size,
    E: Int = scenarioTree.inputEvents.size,
    O: Int = scenarioTree.outputEvents.size,
    X: Int = scenarioTree.inputNames.size,
    Z: Int = scenarioTree.outputNames.size,
    U: Int = scenarioTree.uniqueInputs.size
): BasicVariables {
    /* Core variables */
    val transitionDestination = newIntVar(C, K) { 0..C }
    val transitionInputEvent = newIntVar(C, K) { 0..E }
    val transitionFiring = newBoolVar(C, K, U)
    val firstFired = newIntVar(C, U) { 0..K }
    val notFired = newBoolVar(C, K, U)
    val stateOutputEvent = newIntVar(C) { 0..O }
    val stateAlgorithmBot = newBoolVar(C, Z)
    val stateAlgorithmTop = newBoolVar(C, Z)
    /* Interface variables */
    val actualTransitionFunction = newIntVar(C, E, U) { 0..C }
    /* Mapping variables */
    val mapping = newIntVar(V) { 1..C }

    return BasicVariables(
        scenarioTree = scenarioTree,
        C = C, K = K,
        V = V, E = E, O = O, X = X, Z = Z, U = U,
        transitionDestination = transitionDestination,
        transitionInputEvent = transitionInputEvent,
        transitionFiring = transitionFiring,
        firstFired = firstFired,
        notFired = notFired,
        stateOutputEvent = stateOutputEvent,
        stateAlgorithmTop = stateAlgorithmTop,
        stateAlgorithmBot = stateAlgorithmBot,
        actualTransitionFunction = actualTransitionFunction,
        mapping = mapping
    )
}
