package ru.ifmo.fbsat.core.solver

import java.util.ArrayDeque
import java.util.Deque

class Cardinality(
    val totalizer: BoolVarArray,
    private val solver: Solver
) {
    var upperBound: Int? = null // sum(totalizer) <= upperBound
        private set

    fun updateUpperBoundLessThanOrEqual(newUpperBound: Int?) {
        solver.declareComparatorLessThanOrEqual(totalizer, newUpperBound)
        upperBound = newUpperBound
    }

    fun updateUpperBoundLessThan(newUpperBound: Int?) {
        updateUpperBoundLessThanOrEqual(newUpperBound?.let { it - 1 })
    }
}

fun Solver.declareCardinality(variables: Iterable<Int>): Cardinality =
    Cardinality(
        totalizer = declareTotalizer(variables),
        solver = this
    )

fun Solver.declareCardinality(variables: Sequence<Int>): Cardinality =
    declareCardinality(variables.asIterable())

fun Solver.declareCardinality(block: suspend SequenceScope<Int>.() -> Unit): Cardinality =
    declareCardinality(sequence(block).constrainOnce())

fun Solver.declareTotalizer(variables: Iterable<Int>): BoolVarArray {
    val queue: Deque<List<Int>> = ArrayDeque()

    for (e in variables) {
        queue.addLast(listOf(e))
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
                val c1: List<Int>? = when {
                    sigma == 0 -> null
                    alpha == 0 -> listOf(-b[beta - 1], r[sigma - 1])
                    beta == 0 -> listOf(-a[alpha - 1], r[sigma - 1])
                    else -> listOf(-a[alpha - 1], -b[beta - 1], r[sigma - 1])
                }
                val c2: List<Int>? = when {
                    sigma == m -> null
                    alpha == m1 -> listOf(b[beta], -r[sigma])
                    beta == m2 -> listOf(a[alpha], -r[sigma])
                    else -> listOf(a[alpha], b[beta], -r[sigma])
                }
                c1?.let { clause(*it.toIntArray()) }
                c2?.let { clause(*it.toIntArray()) }
            }
        }
    }

    val totalizer = queue.removeFirst()
    return newBoolVarArray(totalizer.size) { (i) -> totalizer[i - 1] }
}

fun Solver.declareTotalizer(variables: Sequence<Int>): BoolVarArray =
    declareTotalizer(variables.asIterable())

fun Solver.declareTotalizer(block: suspend SequenceScope<Int>.() -> Unit): BoolVarArray =
    declareTotalizer(sequence(block).constrainOnce())

/**
 * Declares cardinality constraint **sum**([totalizer]) <= [x]
 */
fun Solver.declareComparatorLessThanOrEqual(totalizer: BoolVarArray, x: Int?) {
    require(totalizer.shape.size == 1) { "Totalizer must be 1-dimensional." }
    assumptions.clear()
    if (x != null) {
        for (i in (x + 1)..totalizer.shape[0])
            assumptions.add(-totalizer[i])
        // if (x + 1 <= totalizer.shape[0]) {
        //     assumptions.add(-totalizer[x + 1])
        // }
    }
}
