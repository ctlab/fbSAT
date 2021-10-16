package ru.ifmo.fbsat.core.task.single.complete

import com.github.lipen.satlib.core.newBoolVarArray
import com.github.lipen.satlib.core.newIntVarArray
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree

fun Solver.declareCompleteVariables(
    negativeScenarioTree: NegativeScenarioTree,
) {
    context["negativeScenarioTree"] = negativeScenarioTree
    context["negativeTree"] = negativeScenarioTree
    context["negScenarioTree"] = negativeScenarioTree
    context["negTree"] = negativeScenarioTree
    context["negV"] = 0 // must be zero initially
    context["negU"] = 0 // may be anything initially
    context["negUIs"] = emptyList<InputValues>() // must be empty initially
    context["onlyNegUIs"] = emptyList<InputValues>() // Actually `negUIs - posUIs`, but is initially empty
    context["forbiddenLoops"] = mutableSetOf<Pair<Int, Int>>()

    context["negActualTransitionFunction"] = newIntVarArray { emptyList() }
    context["negTransitionTruthTable"] = newBoolVarArray()
    context["negTransitionFiring"] = newBoolVarArray()
    context["negFirstFired"] = newIntVarArray { emptyList() }
    context["negNotFired"] = newBoolVarArray()
    context["negNodeValue"] = newBoolVarArray()
    context["negMapping"] = newIntVarArray { emptyList() }
}
