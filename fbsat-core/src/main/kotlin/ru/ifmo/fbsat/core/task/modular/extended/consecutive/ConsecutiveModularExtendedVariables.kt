package ru.ifmo.fbsat.core.task.modular.extended.consecutive

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.ConsecutiveModularBasicVariables
import ru.ifmo.fbsat.core.task.single.extended.ExtendedVariables
import ru.ifmo.fbsat.core.task.single.extended.declareExtendedVariables

@Suppress("PropertyName")
class ConsecutiveModularExtendedVariables(
    val scenarioTree: ScenarioTree,
    /* Constants */
    val M: Int,
    val C: Int,
    val K: Int,
    val P: Int,
    val V: Int,
    val E: Int,
    val O: Int,
    val X: Int,
    val Z: Int,
    val U: Int,
    /* Modularized ExtendedVariables */
    val modularExtendedVariables: MultiArray<ExtendedVariables>
) {
    /* Cardinality variables */
    var totalizer: IntArray? = null
        internal set
    var maxTotalGuardsSize: Int? = null
        internal set
    val N: Int?
        get() = maxTotalGuardsSize
}

fun Solver.declareConsecutiveModularExtendedVariables(
    basicVars: ConsecutiveModularBasicVariables,
    P: Int
): ConsecutiveModularExtendedVariables = with(basicVars) {
    /* Modularized ExtendedVariables */
    val modularExtendedVariables = MultiArray.create(M) { (m) ->
        declareExtendedVariables(
            basicVars = modularBasicVariables[m],
            P = P
        )
    }

    ConsecutiveModularExtendedVariables(
        scenarioTree = scenarioTree,
        M = M, C = C, K = K, P = P,
        V = V, E = E, O = O, X = X, Z = Z, U = U,
        modularExtendedVariables = modularExtendedVariables
    )
}
