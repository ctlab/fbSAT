package ru.ifmo.fbsat.core.task.single.extforest

import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.DomainVarArray
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newDomainVarArray
import ru.ifmo.fbsat.core.solver.newIntVarArray
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables

@Suppress("PropertyName")
class ExtForestVariables(
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
    val actualTransitionFunction: IntVarArray,
    val transitionDestination: IntVarArray,
    val transitionInputEvent: IntVarArray,
    val transitionFiring: BoolVarArray,
    val firstFired: IntVarArray,
    val notFired: BoolVarArray,
    val stateOutputEvent: IntVarArray,
    val stateAlgorithmTop: BoolVarArray,
    val stateAlgorithmBot: BoolVarArray,
    /* Mapping variables */
    val mapping: IntVarArray,
    /* Guard conditions variables */
    val nodeType: DomainVarArray<NodeType>,
    val nodeInputVariable: IntVarArray,
    val nodeParent: IntVarArray,
    val nodeChild: IntVarArray,
    val nodeValue: BoolVarArray,
    /* Cardinality */
    val cardinality: Cardinality
)

fun Solver.declareExtForestVariables(
    basicVars: BasicVariables,
    P: Int
): ExtForestVariables = with(basicVars) {
    /* Guard conditions variables */
    val nodeType = newDomainVarArray(P) { NodeType.values().asIterable() }
    val nodeInputVariable = newIntVarArray(P) { 0..X }
    val nodeParent = newIntVarArray(P) { (p) ->
        if (p in 1..(C * K))
            listOf(0) // emptyList()
        else
            0 until p
    }
    val nodeChild = newIntVarArray(P) { (p) ->
        if (p in 1..(C * K))
            ((C * K + 1)..P) + 0
        else
            ((p + 1)..P) + 0
    }
    val nodeValue = newBoolVarArray(P, U) { (p, u) ->
        if (p in 1..(C * K)) {
            val (c, k) = p2ck(p, K)
            transitionFiring[c, k, u]
        } else {
            newLiteral()
        }
    }
    /* Cardinality */
    val cardinality = declareCardinality {
        for (p in 1..P)
            yield(nodeType[p] neq NodeType.NONE)
    }

    ExtForestVariables(
        scenarioTree = scenarioTree,
        C = C, K = K, P = P,
        V = V, E = E, O = O, X = X, Z = Z, U = U,
        actualTransitionFunction = actualTransitionFunction,
        transitionDestination = transitionDestination,
        transitionInputEvent = transitionInputEvent,
        transitionFiring = transitionFiring,
        firstFired = firstFired,
        notFired = notFired,
        stateOutputEvent = stateOutputEvent,
        stateAlgorithmTop = stateAlgorithmTop,
        stateAlgorithmBot = stateAlgorithmBot,
        mapping = mapping,
        nodeType = nodeType,
        nodeInputVariable = nodeInputVariable,
        nodeParent = nodeParent,
        nodeChild = nodeChild,
        nodeValue = nodeValue,
        cardinality = cardinality
    )
}
