package ru.ifmo.fbsat.core.task.modular.extended.parallel

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.task.modular.basic.parallel.ParallelModularBasicVariables
import ru.ifmo.fbsat.core.task.single.extended.ExtendedVariables
import ru.ifmo.fbsat.core.task.single.extended.declareExtendedVariables

@Suppress("PropertyName")
class ParallelModularExtendedVariables(
    val scenarioTree: OldPositiveScenarioTree,
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
    val modularExtendedVariables: MultiArray<ExtendedVariables>,
    /* Interface variables */
    val moduleControllingOutputVariable: IntVarArray,
    /* Cardinality */
    val cardinality: Cardinality
)

fun Solver.declareParallelModularExtendedVariables(
    basicVars: ParallelModularBasicVariables,
    P: Int
): ParallelModularExtendedVariables = with(basicVars) {
    /* Modularized ExtendedVariables */
    val modularExtendedVariables = MultiArray.create(M) { (m) ->
        declareExtendedVariables(modularBasicVariables[m], P = P)
    }
    /* Cardinality */
    val cardinality = declareCardinality {
        for (m in 1..M) with(modularExtendedVariables[m]) {
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        yield(nodeType[c, k, p] neq NodeType.NONE)
        }
    }

    ParallelModularExtendedVariables(
        scenarioTree = scenarioTree,
        M = M, C = C, K = K, P = P,
        V = V, E = E, O = O, X = X, Z = Z, U = U,
        modularExtendedVariables = modularExtendedVariables,
        moduleControllingOutputVariable = moduleControllingOutputVariable,
        cardinality = cardinality
    )
}
