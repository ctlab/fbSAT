package ru.ifmo.fbsat.core.task.single.complete

import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.solver.Solver

/*
    Note: do not create getters for negative properties,
    because we need to access old values before recalculating them
*/

// @Suppress("PropertyName")
// class CompleteVariables(
//     val scenarioTree: PositiveScenarioTree,
//     val negativeScenarioTree: NegativeScenarioTree,
//     /* Constants */
//     val C: Int,
//     val K: Int,
//     val P: Int,
//     val V: Int,
//     val E: Int,
//     val O: Int,
//     val X: Int,
//     val Z: Int,
//     val U: Int,
//     /* Core variables */
//     val actualTransitionFunction: IntVarArray,
//     val transitionDestination: IntVarArray,
//     val transitionInputEvent: IntVarArray,
//     val transitionTruthTable: BoolVarArray,
//     val transitionFiring: BoolVarArray,
//     val firstFired: IntVarArray,
//     val notFired: BoolVarArray,
//     val stateOutputEvent: IntVarArray,
//     val stateAlgorithmTop: BoolVarArray,
//     val stateAlgorithmBot: BoolVarArray,
//     /* Mapping variables */
//     val mapping: IntVarArray,
//     /* Guard conditions variables */
//     val nodeType: DomainVarArray<NodeType>,
//     val nodeInputVariable: IntVarArray,
//     val nodeParent: IntVarArray,
//     val nodeChild: IntVarArray,
//     val nodeValue: BoolVarArray
// ) {
//     /* Computable constants */
//     val posUIs: List<InputValues> = scenarioTree.uniqueInputs
//
//     /* Negative constants and variables */
//     var negV: Int = 0
//         internal set
//     var negU: Int = 0
//         internal set
//     var negUIs: List<InputValues> = emptyList()
//         internal set
//     var onlyNegUIs: List<InputValues> = emptyList() // Actually `negUIs - posUIs`, but is initially empty
//         internal set
//
//     // These variables (until negMapping) use U in domain, so must be redeclared for negU
//     lateinit var negTransitionTruthTable: BoolVarArray
//         internal set
//     lateinit var negTransitionFiring: BoolVarArray
//         internal set
//     lateinit var negFirstFired: IntVarArray
//         internal set
//     lateinit var negNotFired: BoolVarArray
//         internal set
//     lateinit var negActualTransitionFunction: IntVarArray
//         internal set
//     lateinit var negNodeValue: BoolVarArray
//         internal set
//
//     lateinit var negMapping: IntVarArray
//         internal set
//     val forbiddenLoops: MutableSet<Pair<Int, Int>> = mutableSetOf()
// }

@Suppress("NAME_SHADOWING")
fun Solver.declareCompleteVariables(
    negativeScenarioTree: NegativeScenarioTree
) {
    val negativeScenarioTree by context(negativeScenarioTree)
}
