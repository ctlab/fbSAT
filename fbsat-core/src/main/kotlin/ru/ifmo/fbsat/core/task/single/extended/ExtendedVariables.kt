package ru.ifmo.fbsat.core.task.single.extended

import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.DomainVarArray
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.literals
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newDomainVarArray
import ru.ifmo.fbsat.core.solver.newIntVarArray
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.utils.Globals

@Suppress("PropertyName")
class ExtendedVariables(
    val scenarioTree: PositiveScenarioTree,
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
    val transitionTruthTable: BoolVarArray,
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

fun Solver.declareExtendedVariables(
    basicVars: BasicVariables,
    P: Int
): ExtendedVariables = with(basicVars) {
    /* Guard conditions variables */
    val nodeType = newDomainVarArray(C, K, P) { NodeType.values().asIterable() }
    val nodeInputVariable = newIntVarArray(C, K, P) { 0..X }
    val nodeParent = newIntVarArray(C, K, P) { (_, _, p) -> 0 until p }
    val nodeChild = newIntVarArray(C, K, P) { (_, _, p) -> ((p + 1)..P) + 0 }
    val nodeValue = newBoolVarArray(C, K, P, U) { (c, k, p, u) ->
        if (p == 1) transitionTruthTable[c, k, u]
        else newLiteral()
    }
    /* Cardinality */
    val cardinality = declareCardinality {
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..P)
                    yield(nodeType[c, k, p] neq NodeType.NONE)
    }

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        comment("nodeType = ${nodeType.literals}")
        comment("nodeInputVariable = ${nodeInputVariable.literals}")
        comment("nodeParent = ${nodeParent.literals}")
        comment("nodeChild = ${nodeChild.literals}")
        comment("nodeValue = ${nodeValue.literals}")
        comment("totalizerN = ${cardinality.totalizer.literals}")
    }

    ExtendedVariables(
        scenarioTree = scenarioTree,
        C = C, K = K, P = P,
        V = V, E = E, O = O, X = X, Z = Z, U = U,
        actualTransitionFunction = actualTransitionFunction,
        transitionDestination = transitionDestination,
        transitionInputEvent = transitionInputEvent,
        transitionTruthTable = transitionTruthTable,
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
