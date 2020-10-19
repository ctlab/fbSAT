package ru.ifmo.fbsat.core.task.modular.extended.consecutive

import ru.ifmo.fbsat.core.solver.Solver

fun Solver.declareConsecutiveModularExtendedVariables(
    P: Int
) {
    // /* Modularized ExtendedVariables */
    // val modularExtendedVariables = MultiArray.create(M) { (m) ->
    //     declareExtendedVariables(modularBasicVariables[m], P = P)
    // }
    // /* Cardinality */
    // val cardinality = declareCardinality {
    //     for (m in 1..M) with(modularExtendedVariables[m]) {
    //         for (c in 1..C)
    //             for (k in 1..K)
    //                 for (p in 1..P)
    //                     yield(nodeType[c, k, p] neq NodeType.NONE)
    //     }
    // }
}
