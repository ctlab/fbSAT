package ru.ifmo.fbsat.core.task.single.extprecomp

import com.github.lipen.satlib.card.declareCardinality
import com.github.lipen.satlib.core.newBoolVarArray
import com.github.lipen.satlib.core.newDomainVarArray
import com.github.lipen.satlib.core.newIntVarArray
import com.github.lipen.satlib.solver.Solver

enum class GuardType(val arity: Int) {
    NoGuard(0), True(0), Var(1), And2(2), Or2(2), And3(3)
}

@Suppress("LocalVariableName")
fun Solver.declareExtPreCompVariables() {
    val C: Int = context["C"]
    val K: Int = context["K"]
    val X: Int = context["X"]
    val U: Int = context["U"]

    /* Guard conditions variables */
    comment("Guard conditions variables")
    val guardType = context("guardType") {
        newDomainVarArray(C, K) { GuardType.values().asIterable() }
    }
    // Why 3? Because we only support at-most-3-arg formulae
    val guardTerminalInputVariable = context("guardTerminalInputVariable") {
        newIntVarArray(C, K, 3) { 0..X }
    }
    val guardTerminalValue = context("guardTerminalValue") {
        newBoolVarArray(C, K, 3, U)
    }
    val guardTerminalNegation = context("guardTerminalNegation") {
        newBoolVarArray(C, K, 3)
    }
    val guardNode = context("guardNode") {
        newBoolVarArray(C, K, 5)
    }

    /* Cardinality */
    comment("Cardinality (N)")
    val cardinalityN = context("cardinalityN") {
        declareCardinality {
            for (c in 1..C)
                for (k in 1..K)
                    for (t in 1..5)
                        yield(guardNode[c, k, t])
        }
    }
}
