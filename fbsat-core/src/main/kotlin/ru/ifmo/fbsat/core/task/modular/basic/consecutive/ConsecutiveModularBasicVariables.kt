package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.BoolVar
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.basic.declareBasicVariables

@Suppress("PropertyName")
class ConsecutiveModularBasicVariables(
    val scenarioTree: ScenarioTree,
    /* Constants */
    val M: Int,
    val C: Int,
    val K: Int,
    val V: Int,
    val E: Int,
    val O: Int,
    val X: Int,
    val Z: Int,
    val U: Int,
    /* Modularized BasicVariables */
    val modularBasicVariables: MultiArray<BasicVariables>,
    /* Mapping variables */
    val modularComputedOutputValue: MultiArray<BoolVar>
) {
    /* Cardinality variables */
    var totalizer: IntArray? = null
        internal set
    var maxTransitions: Int? = null
        internal set
    val T: Int?
        get() = maxTransitions
}

fun Solver.declareConsecutiveModularBasicVariables(
    scenarioTree: ScenarioTree,
    M: Int,
    C: Int,
    K: Int,
    V: Int = scenarioTree.size,
    E: Int = scenarioTree.inputEvents.size,
    O: Int = scenarioTree.outputEvents.size,
    X: Int = scenarioTree.inputNames.size,
    Z: Int = scenarioTree.outputNames.size,
    U: Int = scenarioTree.uniqueInputs.size
): ConsecutiveModularBasicVariables {
    /* Modularized BasicVariables */
    val modularBasicVariables = MultiArray.create(M) { (m) ->
        declareBasicVariables(
            scenarioTree = scenarioTree,
            C = C,
            K = K,
            V = V,
            E = if (m == 1) E else 1,
            O = if (m == M) O else 1,
            X = X,
            Z = Z,
            U = U
        )
    }
    /* Mapping variables */
    val modularComputedOutputValue = MultiArray.create(M) { newBoolVar(V, Z) }

    return ConsecutiveModularBasicVariables(
        scenarioTree = scenarioTree,
        M = M, C = C, K = K,
        V = V, E = E, O = O, X = X, Z = Z, U = U,
        modularBasicVariables = modularBasicVariables,
        modularComputedOutputValue = modularComputedOutputValue
    )
}
