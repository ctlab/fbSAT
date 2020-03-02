package ru.ifmo.fbsat.core.task.single.basic

import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newIntVarArray

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
    /* Cardinality */
    val cardinality: Cardinality
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
    val actualTransitionFunction = newIntVarArray(C, E, U) { 0..C }
    val transitionDestination = newIntVarArray(C, K) { 0..C }
    val transitionInputEvent = newIntVarArray(C, K) { 0..E }
    val transitionFiring = newBoolVarArray(C, K, U)
    val firstFired = newIntVarArray(C, U) { 0..K }
    val notFired = newBoolVarArray(C, K, U)
    val stateOutputEvent = newIntVarArray(C) { 0..O }
    val stateAlgorithmBot = newBoolVarArray(C, Z)
    val stateAlgorithmTop = newBoolVarArray(C, Z)
    /* Mapping variables */
    val mapping = newIntVarArray(V) { 1..C }
    /* Cardinality */
    val cardinality = declareCardinality {
        for (c in 1..C)
            for (k in 1..K)
                yield(transitionDestination[c, k] neq 0)
    }

    return BasicVariables(
        scenarioTree = scenarioTree,
        C = C, K = K,
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
        cardinality = cardinality
    )
}
