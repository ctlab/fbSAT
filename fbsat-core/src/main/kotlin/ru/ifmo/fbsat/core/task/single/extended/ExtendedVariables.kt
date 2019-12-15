package ru.ifmo.fbsat.core.task.single.extended

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
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
    val nodeType = newArray(C, K, P, NodeType.values().size, one = true)
    val nodeInputVariable = newArray(C, K, P, X + 1, one = true)
    val nodeParent = newArray(C, K, P, P + 1, one = true) { (_, _, p, par) ->
        if (par < p || par == P + 1) newVariable()
        else Solver.falseVariable
    }
    val nodeChild = newArray(C, K, P, P + 1, one = true) { (_, _, p, ch) ->
        if (ch > p || ch == P + 1) newVariable()
        else Solver.falseVariable
    }
    val nodeValue = newArray(C, K, P, U) { (c, k, p, u) ->
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
