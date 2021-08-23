package ru.ifmo.fbsat.core.task.single.extprecomp

import com.github.lipen.satlib.card.Cardinality
import com.github.lipen.satlib.core.IntVar
import com.github.lipen.satlib.core.Lit
import com.github.lipen.satlib.core.eq
import com.github.lipen.satlib.core.newBoolVarArray
import com.github.lipen.satlib.core.newDomainVarArray
import com.github.lipen.satlib.core.newIntVarArray
import com.github.lipen.satlib.op.imply
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
    val guardSize = context("guardSize") {
        newIntVarArray(C, K) { 0..5 }
    }

    /* Cardinality */
    comment("Cardinality (N)")
    val cardinalityN = context("cardinalityN") {
        // declareCardinality {
        //     for (c in 1..C)
        //         for (k in 1..K)
        //             for (t in 1..5)
        //                 yield(guardNode[c, k, t])
        // }
        val totalizer = declareTotalizerOneHot(guardSize.values)
        Cardinality(this, totalizer)
    }
}

fun Solver.declareTotalizerOneHot(variables: List<IntVar>): List<Lit> {
    val queue = ArrayDeque<List<Lit>>()

    for (v in variables) {
        check(v.domain.maxOrNull()!! - v.domain.minOrNull()!! + 1 == v.domain.size)
        check(v.domain.minOrNull()!! == 0)
        val W = v.domain.maxOrNull()!!
        val unary = newBoolVarArray(W)
        for (i in 1..W) {
            imply(v eq i, unary[i])
            if (i < W) {
                imply(v eq i, -unary[i + 1])
            }
        }
        queue.addLast(unary.values)
    }

    comment("Totalizer(${queue.size})")

    while (queue.size != 1) {
        val a = queue.removeFirst()
        val b = queue.removeFirst()

        val m1 = a.size
        val m2 = b.size
        val m = m1 + m2

        val r = List(m) { newLiteral() }
        queue.addLast(r)

        for (alpha in 0..m1) {
            for (beta in 0..m2) {
                val sigma = alpha + beta
                // TODO: rewrite without intermediate `c1` and `c2` variables
                val c1: List<Lit>? = when {
                    sigma == 0 -> null
                    alpha == 0 -> listOf(-b[beta - 1], r[sigma - 1])
                    beta == 0 -> listOf(-a[alpha - 1], r[sigma - 1])
                    else -> listOf(-a[alpha - 1], -b[beta - 1], r[sigma - 1])
                }
                val c2: List<Lit>? = when {
                    sigma == m -> null
                    alpha == m1 -> listOf(b[beta], -r[sigma])
                    beta == m2 -> listOf(a[alpha], -r[sigma])
                    else -> listOf(a[alpha], b[beta], -r[sigma])
                }
                c1?.let { addClause(it) }
                c2?.let { addClause(it) }
            }
        }
    }

    val totalizer = queue.removeFirst()
    return totalizer
    // return newBoolVarArray(totalizer.size) { (i) -> totalizer[i - 1] }
}
