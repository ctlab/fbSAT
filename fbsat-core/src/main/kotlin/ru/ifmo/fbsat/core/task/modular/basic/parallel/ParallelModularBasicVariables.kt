package ru.ifmo.fbsat.core.task.modular.basic.parallel

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.newIntVarArray
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.basic.declareBasicVariables

@Suppress("PropertyName")
class ParallelModularBasicVariables(
    val scenarioTree: OldPositiveScenarioTree,
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
    /* Interface variables */
    val moduleControllingOutputVariable: IntVarArray,
    /* Cardinality */
    val cardinality: Cardinality
)

fun Solver.declareParallelModularBasicVariables(
    scenarioTree: OldPositiveScenarioTree,
    M: Int,
    C: Int,
    K: Int,
    V: Int = scenarioTree.size,
    E: Int = scenarioTree.inputEvents.size,
    O: Int = scenarioTree.outputEvents.size,
    X: Int = scenarioTree.inputNames.size,
    Z: Int = scenarioTree.outputNames.size,
    U: Int = scenarioTree.uniqueInputs.size
): ParallelModularBasicVariables {
    /* Modularized BasicVariables */
    val modularBasicVariables = MultiArray.create(M) { (m) ->
        declareBasicVariables(
            scenarioTree = scenarioTree,
            C = C, K = K,
            V = V, E = E, O = O, X = X, Z = Z, U = U
        )
    }
    /* Interface variables */
    val moduleControllingOutputVariable = newIntVarArray(Z) { 1..M }
    /* Cardinality */
    val cardinality = declareCardinality {
        for (m in 1..M) with(modularBasicVariables[m]) {
            for (c in 1..C)
                for (k in 1..K)
                    yield(transitionDestination[c, k] neq 0)
        }
    }

    return ParallelModularBasicVariables(
        scenarioTree = scenarioTree,
        M = M, C = C, K = K,
        V = V, E = E, O = O, X = X, Z = Z, U = U,
        modularBasicVariables = modularBasicVariables,
        moduleControllingOutputVariable = moduleControllingOutputVariable,
        cardinality = cardinality
    )
}
