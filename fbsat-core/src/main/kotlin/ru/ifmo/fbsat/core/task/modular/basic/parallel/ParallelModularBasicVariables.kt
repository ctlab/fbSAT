package ru.ifmo.fbsat.core.task.modular.basic.parallel

import com.github.lipen.satlib.card.declareCardinality
import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.neq
import com.github.lipen.satlib.core.newIntVarArray
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.declareModularContext
import ru.ifmo.fbsat.core.solver.forEachModularContext
import ru.ifmo.fbsat.core.task.single.basic.declareBasicVariables
import ru.ifmo.fbsat.core.utils.Globals

@Suppress("LocalVariableName")
fun Solver.declareParallelModularBasicVariables(
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
    forEachModularContext {
        declareBasicVariables(
            positiveScenarioTree = scenarioTree,
            C = C, K = K,
            V = V, E = E, O = O, X = X, Z = Z, U = U
        )
    }

    /* Interface variables */
    val moduleControllingOutputVariable = context("moduleControllingOutputVariable") {
        newIntVarArray(Z) { 1..M }
    }

    /* Cardinality */
    comment("Cardinality (T)")
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
    if (Globals.IS_ENCODE_CONJUNCTIVE_GUARDS) {
        comment("Cardinality (A)")
        val cardinalityA = context("cardinalityA") {
            declareCardinality {
                @Suppress("NAME_SHADOWING")
                forEachModularContext {
                    val C: Int = context["C"]
                    val K: Int = context["K"]
                    val X: Int = context["X"]
                    val inputVariableUsed: BoolVarArray = context["inputVariableUsed"]
                    for (c in 1..C)
                        for (k in 1..K)
                            for (x in 1..X)
                                yield(inputVariableUsed[x])
                }
            }
        }
    }
}
