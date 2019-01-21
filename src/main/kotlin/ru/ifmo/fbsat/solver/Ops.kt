package ru.ifmo.fbsat.solver

import ru.ifmo.fbsat.utils.IntMultiArray

fun Solver.declareAtLeastOne(range: IntRange, array: IntMultiArray, vararg index: Int) {
    clause(range.asSequence().map {
        @Suppress("ReplaceGetOrSet")
        array.get(*index, it)
    })
}

fun Solver.declareAtMostOne(range: IntRange, array: IntMultiArray, vararg index: Int) {
    for (a in range) {
        for (b in (a + 1)..(range.last)) {
            @Suppress("ReplaceGetOrSet")
            clause(-array.get(*index, a), -array.get(*index, b))
        }
    }
}

fun Solver.declareExactlyOne(range: IntRange, array: IntMultiArray, vararg index: Int) {
    declareAtLeastOne(range, array, *index)
    declareAtMostOne(range, array, *index)
}

/**
 * [lhs] => [rhs]
 */
fun Solver.declareImply(lhs: Int, rhs: Int) {
    clause(-lhs, rhs)
}

/**
 * [base] => ([lhs] <=> [rhs])
 */
fun Solver.declareImplyIff(base: Int, lhs: Int, rhs: Int) {
    clause(-base, -lhs, rhs)
    clause(-base, lhs, -rhs)
}

/**
 * [base] => ([lhs] <=> AND([rhs]))
 */
fun Solver.declareImplyIffAnd(base: Int, lhs: Int, rhs: Sequence<Int>) {
    clause(sequence {
        yield(-base)
        yield(lhs)
        for (x in rhs) {
            clause(-base, -lhs, x)
            yield(-x)
        }
    })
}

/**
 * [base] => ([lhs] <=> AND([rhs]))
 */
fun Solver.declareImplyIffAnd(base: Int, lhs: Int, vararg rhs: Int) = declareImplyIffAnd(base, lhs, rhs.asSequence())

/**
 * [base] => ([lhs] <=> OR([rhs]))
 */
fun Solver.declareImplyIffOr(base: Int, lhs: Int, rhs: Sequence<Int>) {
    clause(sequence {
        yield(-base)
        yield(-lhs)
        for (x in rhs) {
            clause(-base, lhs, -x)
            yield(x)
        }
    })
}

/**
 * [base] => ([lhs] <=> OR([rhs]))
 */
fun Solver.declareImplyIffOr(base: Int, lhs: Int, vararg rhs: Int) = declareImplyIffOr(base, lhs, rhs.asSequence())

/**
 * [lhs] <=> [rhs]
 */
fun Solver.declareIff(lhs: Int, rhs: Int) {
    declareImply(lhs, rhs)
    declareImply(rhs, lhs)
}

/**
 * [lhs] <=> AND([rhs])
 */
fun Solver.declareIffAnd(lhs: Int, rhs: Sequence<Int>) {
    for (x in rhs)
        clause(-lhs, x)
    clause(sequenceOf(lhs) + rhs.map { -it })
}

/**
 * [lhs] <=> AND([rhs])
 */
fun Solver.declareIffAnd(lhs: Int, vararg rhs: Int) = declareIffAnd(lhs, rhs.asSequence())

/**
 * [lhs] <=> OR([rhs])
 */
fun Solver.declareIffOr(lhs: Int, rhs: Sequence<Int>) {
    for (x in rhs)
        clause(lhs, -x)
    clause(sequenceOf(-lhs) + rhs)
}

/**
 * [lhs] <=> OR([rhs])
 */
fun Solver.declareIffOr(lhs: Int, vararg rhs: Int) = declareIffOr(lhs, rhs.asSequence())
