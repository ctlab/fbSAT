package ru.ifmo.fbsat.core.task.single.complete

import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.BoolVar
import ru.ifmo.fbsat.core.solver.IntVar
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.Var
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
    val mapping: IntVar,
    /* Guard conditions variables */
    val nodeType: Var<NodeType>,
    val nodeInputVariable: IntVar,
    val nodeParent: IntVar,
    val nodeChild: IntVar,
    val nodeValue: BoolVar
) {
    /* Computable constants */
    val posUIs: List<InputValues> = scenarioTree.uniqueInputs

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
    lateinit var negTransitionFiring: BoolVar
        internal set
    lateinit var negFirstFired: IntVar
        internal set
    lateinit var negNotFired: BoolVar
        internal set
    lateinit var negActualTransitionFunction: IntVar
        internal set
    lateinit var negNodeValue: BoolVar
        internal set
    lateinit var negMapping: IntVar
        internal set
    val forbiddenLoops: MutableSet<Pair<Int, Int>> = mutableSetOf()
}

fun Solver.declareCompleteVariables(
    extendedVars: ExtendedVariables,
    negativeScenarioTree: NegativeScenarioTree
): CompleteVariables = with(extendedVars) {
    CompleteVariables(
        scenarioTree = scenarioTree,
        negativeScenarioTree = negativeScenarioTree,
        C = C, K = K, P = P,
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
        mapping = mapping,
        nodeType = nodeType,
        nodeInputVariable = nodeInputVariable,
        nodeParent = nodeParent,
        nodeChild = nodeChild,
        nodeValue = nodeValue
    )
}
