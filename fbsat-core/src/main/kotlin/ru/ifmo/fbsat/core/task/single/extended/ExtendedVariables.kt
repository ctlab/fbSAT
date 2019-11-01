package ru.ifmo.fbsat.core.task.single.extended

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables

@Suppress("PropertyName")
class ExtendedVariables(
    val scenarioTree: ScenarioTree,
    /* Constants */
    val C: Int,
    val K: Int,
    val P: Int,
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
    val transitionFunction: IntMultiArray,
    val actualTransitionFunction: IntMultiArray,
    val outputEventFunction: IntMultiArray,
    val algorithmFunctionTop: IntMultiArray,
    val algorithmFunctionBot: IntMultiArray,
    /* Mapping variables */
    val mapping: IntMultiArray,
    /* Guard conditions variables */
    val nodeType: IntMultiArray,
    val nodeInputVariable: IntMultiArray,
    val nodeParent: IntMultiArray,
    val nodeChild: IntMultiArray,
    val nodeValue: IntMultiArray
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
    var maxTotalGuardsSize: Int? = null
        internal set
    val N: Int?
        get() = maxTotalGuardsSize

    constructor(
        basicVars: BasicVariables,
        P: Int,
        nodeType: IntMultiArray,
        nodeInputVariable: IntMultiArray,
        nodeParent: IntMultiArray,
        nodeChild: IntMultiArray,
        nodeValue: IntMultiArray
    ) : this(
        scenarioTree = basicVars.scenarioTree,
        C = basicVars.C,
        K = basicVars.K,
        P = P,
        transitionDestination = basicVars.transitionDestination,
        transitionInputEvent = basicVars.transitionInputEvent,
        transitionFiring = basicVars.transitionFiring,
        firstFired = basicVars.firstFired,
        notFired = basicVars.notFired,
        stateOutputEvent = basicVars.stateOutputEvent,
        stateAlgorithmTop = basicVars.stateAlgorithmTop,
        stateAlgorithmBot = basicVars.stateAlgorithmBot,
        transitionFunction = basicVars.transitionFunction,
        actualTransitionFunction = basicVars.actualTransitionFunction,
        outputEventFunction = basicVars.outputEventFunction,
        algorithmFunctionTop = basicVars.algorithmFunctionTop,
        algorithmFunctionBot = basicVars.algorithmFunctionBot,
        mapping = basicVars.mapping,
        nodeType = nodeType,
        nodeInputVariable = nodeInputVariable,
        nodeParent = nodeParent,
        nodeChild = nodeChild,
        nodeValue = nodeValue
    )
}
