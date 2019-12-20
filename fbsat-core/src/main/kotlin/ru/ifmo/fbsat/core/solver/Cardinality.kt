package ru.ifmo.fbsat.core.solver

import java.util.ArrayDeque
import java.util.Deque

fun Solver.declareTotalizer(variables: Sequence<Int>): IntArray {
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

        val r = List(m) { newVariable() }
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

    return queue.removeFirst().toIntArray()
}

fun Solver.declareTotalizer(block: suspend SequenceScope<Int>.() -> Unit): IntArray =
    declareTotalizer(sequence(block).constrainOnce())

/**
 * Declares cardinality constraint `sum(totalizer) <= x`
 * @param[declared] previously declared upper bound.
 */
fun Solver.declareComparatorLessThanOrEqual(totalizer: IntArray, x: Int, declared: Int? = null) {
    val max = declared ?: totalizer.size
    comment("Comparator(<=$x up to $max)")
    for (i in max downTo x + 1) {
        clause(-totalizer[i - 1]) // Note: totalizer is zero-based
    }
}
