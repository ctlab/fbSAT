package ru.ifmo.fbsat.core.task.single.basic

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
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
    val transitionDestination: IntMultiArray,
    val transitionInputEvent: IntMultiArray,
    val transitionFiring: IntMultiArray,
    val firstFired: IntMultiArray,
    val notFired: IntMultiArray,
    val stateOutputEvent: IntMultiArray,
    val stateAlgorithmTop: IntMultiArray,
    val stateAlgorithmBot: IntMultiArray,
    /* Interface variables */
    val actualTransitionFunction: IntMultiArray,
    /* Mapping variables */
    val mapping: IntMultiArray
) {
    /* Cardinality variables */
    var totalizer: IntArray? = null
        internal set
    var maxTransitions: Int? = null
        internal set
    val T: Int?
        get() = maxTransitions
}

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
    val transitionDestination = newArray(C, K, C + 1, one = true)
    val transitionInputEvent = newArray(C, K, E + 1, one = true)
    val transitionFiring = newArray(C, K, U)
    val firstFired = newArray(C, U, K + 1)
    val notFired = newArray(C, K, U)
    val stateOutputEvent = newArray(C, O + 1, one = true)
    val stateAlgorithmBot = newArray(C, Z)
    val stateAlgorithmTop = newArray(C, Z)
    /* Interface variables */
    val actualTransitionFunction = newArray(C, E, U, C + 1, one = true)
    /* Mapping variables */
    val mapping = newArray(V, C, one = true)

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
