package ru.ifmo.fbsat.core.task.distributed

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.basic.declareBasicVariables

class DistributedBasicVariables(
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
    /* Cardinality */
    val cardinality: Cardinality
)

fun Solver.declareDistributedBasicVariables(
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
): DistributedBasicVariables {
    /* Modularized BasicVariables */
    val modularBasicVariables = MultiArray.create(M) { (m) ->
        declareBasicVariables(
            scenarioTree = scenarioTree,
            C = C, K = K,
            V = V, E = E, O = O, X = X, Z = Z, U = U
        )
    }
    /* Cardinality */
    val cardinality = declareCardinality {
        for (m in 1..M) with(modularBasicVariables[m]) {
            for (c in 1..C)
                for (k in 1..K)
                    yield(transitionDestination[c, k] neq 0)
        }
    }

    return DistributedBasicVariables(
        scenarioTree = scenarioTree,
        M = M, C = C, K = K,
        V = V, E = E, O = O, X = X, Z = Z, U = U,
        modularBasicVariables = modularBasicVariables,
        cardinality = cardinality
    )
}
