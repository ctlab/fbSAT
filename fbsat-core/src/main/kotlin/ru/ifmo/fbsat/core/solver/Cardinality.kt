package ru.ifmo.fbsat.core.solver

import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import java.util.ArrayDeque
import java.util.Deque

class Cardinality(
    val totalizer: BoolVarArray,
    private val solver: Solver,
    private val useAssumptions: Boolean = false,
) {
    init { check(!useAssumptions || solver is AssumptionSupportable) { "Solver doesnt' support assumptions" } }
    var upperBound: Int? = null // sum(totalizer) <= upperBound
        private set

    fun updateUpperBoundLessThanOrEqual(newUpperBound: Int?) {
        // new bound should be less than previous or we use assumptions and they were reset in solver
        upperBound?.let { curUpperBound ->
            check(newUpperBound != null &&
                (newUpperBound <= curUpperBound ||
                    (useAssumptions && solver is AssumptionSupportable && solver.getAssumptions().isEmpty()))) { "Cannot soften UB" }
        }

        if (newUpperBound == null) return

        if (solver is AssumptionSupportable && solver.getAssumptions().isEmpty()) {
            upperBound = null
        }

        solver.declareComparatorLessThanOrEqual(totalizer, newUpperBound, upperBound, useAssumptions)
        upperBound = newUpperBound
    }

    fun updateUpperBoundLessThan(newUpperBound: Int?) {
        updateUpperBoundLessThanOrEqual(newUpperBound?.let { it - 1 })
    }
}

fun Solver.declareCardinality(
    variables: Iterable<Literal>,
    useAssumptions: Boolean = false
): Cardinality =
    Cardinality(
        totalizer = declareTotalizer(variables),
        solver = this,
        useAssumptions = useAssumptions,
    )

fun Solver.declareCardinality(
    variables: Sequence<Literal>,
    useAssumptions: Boolean = false,
): Cardinality =
    declareCardinality(variables.asIterable(), useAssumptions)

fun Solver.declareCardinality(
    variables: SequenceScopeLiteral,
    useAssumptions: Boolean = false,
): Cardinality =
    declareCardinality(sequence(variables).constrainOnce(), useAssumptions)

fun Solver.declareCardinality(
    variables: SequenceScopeLiteral,
): Cardinality = declareCardinality(variables, false)

fun Solver.declareTotalizer(variables: Iterable<Literal>): BoolVarArray {
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
                c1?.let { clause(it) }
                c2?.let { clause(it) }
            }
        }
    }

    val totalizer = queue.removeFirst()
    return newBoolVarArray(totalizer.size) { (i) -> totalizer[i - 1] }
}

fun Solver.declareTotalizer(variables: Sequence<Literal>): BoolVarArray =
    declareTotalizer(variables.asIterable())

fun Solver.declareTotalizer(variables: SequenceScopeLiteral): BoolVarArray =
    declareTotalizer(sequence(variables).constrainOnce())

/**
 * Declares cardinality constraint `sum(totalizer) <= x`
 * @param[declared] previously declared upper bound.
 */
fun Solver.declareComparatorLessThanOrEqual(
    totalizer: BoolVarArray,
    x: Int,
    declared: Int? = null,
    useAssumptions: Boolean = false,
) {
    require(totalizer.shape.size == 1) { "Totalizer must be 1-dimensional." }
    val max = declared ?: totalizer.values.size
    comment("Comparator(<=$x up to $max)")
    for (i in max downTo x + 1) {
        when (useAssumptions && this is AssumptionSupportable) {
            true -> addAssumptions(-totalizer[i])
            false -> clause(-totalizer[i])
        }
    }
}
