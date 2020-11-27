package ru.ifmo.fbsat.core.task.single.extended

import com.github.lipen.satlib.solver.Solver
import com.github.lipen.satlib.utils.BoolVarArray
import com.github.lipen.satlib.utils.newBoolVarArray
import com.github.lipen.satlib.utils.newDomainVarArray
import com.github.lipen.satlib.utils.newIntVarArray
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.literals
import ru.ifmo.fbsat.core.utils.Globals

@Suppress("LocalVariableName")
fun Solver.declareExtendedVariables(
    P: Int,
) {
    val C: Int = context["C"]
    val K: Int = context["K"]
    context["P"] = P
    val X: Int = context["X"]
    val U: Int = context["U"]
    val transitionTruthTable: BoolVarArray = context["transitionTruthTable"]

    /* Guard conditions variables */
    comment("Guard conditions variables")
    val nodeType = context("nodeType") {
        newDomainVarArray(C, K, P) { NodeType.values().asIterable() }
    }
    val nodeInputVariable = context("nodeInputVariable") {
        newIntVarArray(C, K, P) { 0..X }
    }
    val nodeParent = context("nodeParent") {
        newIntVarArray(C, K, P) { (_, _, p) -> 0 until p }
    }
    val nodeChild = context("nodeChild") {
        newIntVarArray(C, K, P) { (_, _, p) -> ((p + 1)..P) + 0 }
    }
    val nodeValue = context("nodeValue") {
        newBoolVarArray(C, K, P, U) { (c, k, p, u) ->
            if (p == 1) transitionTruthTable[c, k, u]
            else newLiteral()
        }
    }

    /* Cardinality */
    comment("Cardinality (N)")
    val cardinalityN = context("cardinalityN") {
        declareCardinality {
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        yield(nodeType[c, k, p] neq NodeType.NONE)
        }
    }

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        comment("nodeType = ${nodeType.literals}")
        comment("nodeInputVariable = ${nodeInputVariable.literals}")
        comment("nodeParent = ${nodeParent.literals}")
        comment("nodeChild = ${nodeChild.literals}")
        comment("nodeValue = ${nodeValue.literals}")
        comment("totalizerN = ${cardinalityN.totalizer.literals}")
    }
}
