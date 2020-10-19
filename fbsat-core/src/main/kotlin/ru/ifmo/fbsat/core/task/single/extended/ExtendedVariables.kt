package ru.ifmo.fbsat.core.task.single.extended

import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.literals
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newDomainVarArray
import ru.ifmo.fbsat.core.solver.newIntVarArray
import ru.ifmo.fbsat.core.utils.Globals

@Suppress("LocalVariableName", "NAME_SHADOWING")
fun Solver.declareExtendedVariables(
    P: Int
) {
    val C: Int by context
    val K: Int by context
    val P by context(P)
    val X: Int by context
    val U: Int by context
    val transitionTruthTable: BoolVarArray by context

    /* Guard conditions variables */
    comment("Guard conditions variables")
    val nodeType by context(newDomainVarArray(C, K, P) { NodeType.values().asIterable() })
    val nodeInputVariable by context(newIntVarArray(C, K, P) { 0..X })
    val nodeParent by context(newIntVarArray(C, K, P) { (_, _, p) -> 0 until p })
    val nodeChild by context(newIntVarArray(C, K, P) { (_, _, p) -> ((p + 1)..P) + 0 })
    val nodeValue by context(newBoolVarArray(C, K, P, U) { (c, k, p, u) ->
        if (p == 1) transitionTruthTable[c, k, u]
        else newLiteral()
    })

    /* Cardinality */
    comment("Cardinality (N)")
    val cardinalityN by context(declareCardinality {
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..P)
                    yield(nodeType[c, k, p] neq NodeType.NONE)
    })

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        comment("nodeType = ${nodeType.literals}")
        comment("nodeInputVariable = ${nodeInputVariable.literals}")
        comment("nodeParent = ${nodeParent.literals}")
        comment("nodeChild = ${nodeChild.literals}")
        comment("nodeValue = ${nodeValue.literals}")
        comment("totalizerN = ${cardinalityN.totalizer.literals}")
    }
}
