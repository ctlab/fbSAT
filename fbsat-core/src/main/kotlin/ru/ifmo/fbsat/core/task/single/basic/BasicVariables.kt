package ru.ifmo.fbsat.core.task.single.basic

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree

@Suppress("PropertyName")
class BasicVariables(
    val scenarioTree: ScenarioTree,
    /* Constants */
    val C: Int,
    val K: Int,
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
    /* Computable constants */
    val V: Int = scenarioTree.size
    val E: Int = scenarioTree.inputEvents.size
    val O: Int = scenarioTree.outputEvents.size
    val X: Int = scenarioTree.inputNames.size
    val Z: Int = scenarioTree.outputNames.size
    val U: Int = scenarioTree.uniqueInputs.size

    /* Cardinality variables */
    var totalizer: IntArray? = null
        internal set
    var maxTransitions: Int? = null
        internal set
    val T: Int?
        get() = maxTransitions
}
