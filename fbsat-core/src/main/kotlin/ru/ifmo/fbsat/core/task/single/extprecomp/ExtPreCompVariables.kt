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
    // Formulae:
    //  - tautology (unconditional 1)
    //  - positive variable
    //  - negative variable
    //  - conjunction of 2 positive variables
    //  - conjunction of 2 negative variables
    //  - conjunction of 1 positive and 1 negative variable
    //  - conjunction of 1 negative and 1 positive variable
    //  - disjunction of 2 positive variables
    //  - conjunction of 3 position variables
    // val F = 1 + 2 * X + 5 * (X * (X - 1) / 2) + (X * (X - 1) * (X - 2) / 6)
    // val transitionGuardFormula = context("transitionGuardFormula") {
    //     // 0 means "no guard"
    //     newIntVarArray(C, K) { 0..F }
    // }
    // val transitionGuardFormulaNode = context("transitionGuardFormulaNode") {
    //     newBoolVarArray(C, K, 5)
    // }

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
                // yield(transitionGuardFormula[c, k] neq 0)
                //     for (t in 1..5)
                //         yield(transitionGuardFormulaNode[c, k, t])
                // yield(guardType[c, k] neq GuardType.NoGuard)
                    for (t in 1..5)
                        yield(guardNode[c, k, t])
        }
    }
}
