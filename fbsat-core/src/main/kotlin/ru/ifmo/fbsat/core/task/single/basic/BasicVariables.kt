package ru.ifmo.fbsat.core.task.single.basic

import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.literals
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newIntVarArray
import ru.ifmo.fbsat.core.utils.Globals

@Suppress("LocalVariableName", "NAME_SHADOWING")
fun Solver.declareBasicVariables(
    scenarioTree: PositiveScenarioTree,
    C: Int,
    K: Int,
    V: Int = scenarioTree.size,
    E: Int = scenarioTree.inputEvents.size,
    O: Int = scenarioTree.outputEvents.size,
    X: Int = scenarioTree.inputNames.size,
    Z: Int = scenarioTree.outputNames.size,
    U: Int = scenarioTree.uniqueInputs.size
) {
    val scenarioTree by context(scenarioTree)
    context["positiveScenarioTree"] = scenarioTree
    val C by context(C)
    val K by context(K)
    val V by context(V)
    val E by context(E)
    val O by context(O)
    val X by context(X)
    val Z by context(Z)
    val U by context(U)

    /* Core variables */
    comment("Core variables")
    val actualTransitionFunction by context(newIntVarArray(C, E, U) { 0..C })
    val transitionDestination by context(newIntVarArray(C, K) { 0..C })
    val transitionInputEvent by context(newIntVarArray(C, K) { 0..E })
    val transitionTruthTable by context(newBoolVarArray(C, K, U))
    val transitionFiring by context(newBoolVarArray(C, K, E, U))
    val firstFired by context(newIntVarArray(C, E, U) { 0..K })
    val notFired by context(newBoolVarArray(C, K, E, U))
    val stateOutputEvent by context(newIntVarArray(C) { 0..O })
    val stateAlgorithmBot by context(newBoolVarArray(C, Z))
    val stateAlgorithmTop by context(newBoolVarArray(C, Z))

    /* Mapping variables */
    comment("Mapping variables")
    val mapping by context(newIntVarArray(V) { 1..C })

    /* Cardinality */
    comment("Cardinality (T)")
    val cardinalityT by context(declareCardinality {
        for (c in 1..C)
            for (k in 1..K)
                yield(transitionDestination[c, k] neq 0)
    })

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        comment("actualTransitionFunction = ${actualTransitionFunction.literals}")
        comment("transitionDestination = ${transitionDestination.literals}")
        comment("transitionInputEvent = ${transitionInputEvent.literals}")
        comment("transitionTruthTable = ${transitionTruthTable.literals}")
        comment("transitionFiring = ${transitionFiring.literals}")
        comment("firstFired = ${firstFired.literals}")
        comment("notFired = ${notFired.literals}")
        comment("stateOutputEvent = ${stateOutputEvent.literals}")
        comment("stateAlgorithmBot = ${stateAlgorithmBot.literals}")
        comment("stateAlgorithmTop = ${stateAlgorithmTop.literals}")
        comment("mapping = ${mapping.literals}")
        comment("totalizerT = ${cardinalityT.totalizer.literals}")
    }
}
