package ru.ifmo.fbsat.core.task.single.extended

import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.BoolVar
import ru.ifmo.fbsat.core.solver.IntVar
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.Var
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables

@Suppress("PropertyName")
class ExtendedVariables(
    val scenarioTree: ScenarioTree,
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
    /* Cardinality variables */
    var totalizer: IntArray? = null
        internal set
    var maxTotalGuardsSize: Int? = null
        internal set
    val N: Int?
        get() = maxTotalGuardsSize
}

fun Solver.declareExtendedVariables(
    basicVars: BasicVariables,
    P: Int
): ExtendedVariables = with(basicVars) {
    /* Guard conditions variables */
    val nodeType = newVar(C, K, P) { NodeType.values().asIterable() }
    val nodeInputVariable = newIntVar(C, K, P) { 0..X }
    val nodeParent = newIntVar(C, K, P) { (_, _, p) -> 0 until p }
    val nodeChild = newIntVar(C, K, P) { (_, _, p) -> ((p + 1)..P) + 0 }
    val nodeValue = newBoolVar(C, K, P, U) { (c, k, p, u) ->
        if (p == 1) transitionFiring[c, k, u]
        else newVariable()
    }

    ExtendedVariables(
        scenarioTree = scenarioTree,
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
