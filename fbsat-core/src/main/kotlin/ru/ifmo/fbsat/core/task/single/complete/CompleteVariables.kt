package ru.ifmo.fbsat.core.task.single.complete

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.single.extended.ExtendedVariables

/*
    Note: do not create getters for negative properties,
    because we need to access old values before recalculating them
*/

@Suppress("PropertyName")
class CompleteVariables(
    val scenarioTree: ScenarioTree,
    val negativeScenarioTree: NegativeScenarioTree,
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
    val actualTransitionFunction: IntMultiArray,
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
    val posUIs: List<InputValues> = scenarioTree.uniqueInputs
    val U: Int = posUIs.size

    /* Cardinality variables */
    var totalizer: IntArray? = null
        internal set
    var maxTotalGuardsSize: Int? = null
        internal set
    val N: Int?
        get() = maxTotalGuardsSize

    /* Negative constants and variables */
    var negV: Int = 0
        internal set
    var negU: Int = 0
        internal set
    var negUIs: List<InputValues> = emptyList()
        internal set
    var onlyNegUIs: List<InputValues> = emptyList() // Actually `negUIs - posUIs`, but is initially empty
        internal set
    // These variables (until negMapping) use U in domain, so must be redeclared for negU
    lateinit var negTransitionFiring: IntMultiArray
        internal set
    lateinit var negFirstFired: IntMultiArray
        internal set
    lateinit var negNotFired: IntMultiArray
        internal set
    lateinit var negActualTransitionFunction: IntMultiArray
        internal set
    lateinit var negNodeValue: IntMultiArray
        internal set
    lateinit var negMapping: IntMultiArray
        internal set
    val forbiddenLoops: MutableSet<Pair<Int, Int>> = mutableSetOf()

    constructor(
        extVars: ExtendedVariables,
        negativeScenarioTree: NegativeScenarioTree
    ) : this(
        scenarioTree = extVars.scenarioTree,
        negativeScenarioTree = negativeScenarioTree,
        C = extVars.C,
        K = extVars.K,
        P = extVars.P,
        transitionDestination = extVars.transitionDestination,
        transitionInputEvent = extVars.transitionInputEvent,
        transitionFiring = extVars.transitionFiring,
        firstFired = extVars.firstFired,
        notFired = extVars.notFired,
        stateOutputEvent = extVars.stateOutputEvent,
        stateAlgorithmTop = extVars.stateAlgorithmTop,
        stateAlgorithmBot = extVars.stateAlgorithmBot,
        actualTransitionFunction = extVars.actualTransitionFunction,
        mapping = extVars.mapping,
        nodeType = extVars.nodeType,
        nodeInputVariable = extVars.nodeInputVariable,
        nodeParent = extVars.nodeParent,
        nodeChild = extVars.nodeChild,
        nodeValue = extVars.nodeValue
    )
}
