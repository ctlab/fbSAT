package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.card.declareCardinality
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.neq
import com.github.lipen.satlib.core.newBoolVarArray
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.declareModularContext
import ru.ifmo.fbsat.core.solver.forEachModularContext
import ru.ifmo.fbsat.core.task.single.basic.declareBasicVariables

@Suppress("LocalVariableName")
fun Solver.declareConsecutiveModularBasicVariables(
    scenarioTree: PositiveScenarioTree,
    M: Int,
    C: Int,
    K: Int,
    V: Int = scenarioTree.size,
    E: Int = scenarioTree.inputEvents.size,
    O: Int = scenarioTree.outputEvents.size,
    X: Int = scenarioTree.inputNames.size,
    Z: Int = scenarioTree.outputNames.size,
    U: Int = scenarioTree.uniqueInputs.size,
) {
    context["positiveScenarioTree"] = scenarioTree
    context["scenarioTree"] = scenarioTree
    context["tree"] = scenarioTree
    context["M"] = M
    context["C"] = C
    context["K"] = K
    context["V"] = V
    context["E"] = E
    context["O"] = O
    context["X"] = X
    context["Z"] = Z
    context["U"] = U

    /* Modular */
    declareModularContext(M)
    forEachModularContext { m ->
        declareBasicVariables(
            positiveScenarioTree = scenarioTree,
            C = C,
            K = K,
            V = V,
            // TODO: should be:
            //   E = E,
            //   O = if (m == M) O else E,
            E = if (m == 1) E else 1,
            O = if (m == M) O else 1,
            X = X,
            Z = Z,
            U = U
        )
    }

    /* Mapping variables */
    val modularComputedOutputValue = context("modularComputedOutputValue") {
        MultiArray.new(M) { newBoolVarArray(V, Z) }
    }

    /* Cardinality */
    val cardinalityT = context("cardinalityT") {
        declareCardinality {
            @Suppress("NAME_SHADOWING")
            forEachModularContext {
                val C: Int = context["C"]
                val K: Int = context["K"]
                val transitionDestination: IntVarArray = context["transitionDestination"]
                for (c in 1..C)
                    for (k in 1..K)
                        yield(transitionDestination[c, k] neq 0)
            }
        }
    }
}
