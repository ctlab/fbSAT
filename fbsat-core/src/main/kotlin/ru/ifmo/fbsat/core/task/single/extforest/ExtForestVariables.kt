package ru.ifmo.fbsat.core.task.single.extforest

import ru.ifmo.fbsat.core.solver.Solver

fun Solver.declareExtForestVariables(
    P: Int
) {
    // /* Guard conditions variables */
    // val nodeType = newDomainVarArray(P) { NodeType.values().asIterable() }
    // val nodeInputVariable = newIntVarArray(P) { 0..X }
    // val nodeParent = newIntVarArray(P) { (p) ->
    //     if (p in 1..(C * K))
    //         listOf(0) // emptyList()
    //     else
    //         0 until p
    // }
    // val nodeChild = newIntVarArray(P) { (p) ->
    //     if (p in 1..(C * K))
    //         ((C * K + 1)..P) + 0
    //     else
    //         ((p + 1)..P) + 0
    // }
    // val nodeValue = newBoolVarArray(P, U) { (p, u) ->
    //     if (p in 1..(C * K)) {
    //         val (c, k) = p2ck(p, K)
    //         transitionFiring[c, k, u]
    //     } else {
    //         newLiteral()
    //     }
    // }
    // /* Cardinality */
    // val cardinality = declareCardinality {
    //     for (p in 1..P)
    //         yield(nodeType[p] neq NodeType.NONE)
    // }
}
