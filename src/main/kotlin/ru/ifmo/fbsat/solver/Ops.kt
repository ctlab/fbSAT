package ru.ifmo.fbsat.solver

import ru.ifmo.fbsat.utils.IntMultiArray

fun Solver.atLeastOne(range: IntRange, array: IntMultiArray, vararg index: Int) {
    clause(range.asSequence().map {
        @Suppress("ReplaceGetOrSet")
        array.get(*index, it)
    })
}

fun Solver.atMostOne(literals: List<Int>) {
    for (a in literals.indices)
        for (b in (a + 1) until literals.size)
            clause(-literals[a], -literals[b])
}

fun Solver.atMostOne(range: IntRange, array: IntMultiArray, vararg index: Int) {
    for (a in range) {
        for (b in (a + 1)..(range.last)) {
            @Suppress("ReplaceGetOrSet")
            clause(-array.get(*index, a), -array.get(*index, b))
        }
    }
}

fun Solver.exactlyOne(range: IntRange, array: IntMultiArray, vararg index: Int) {
    atLeastOne(range, array, *index)
    atMostOne(range, array, *index)
}


/**
 * [lhs] => [rhs]
 */
fun Solver.imply(lhs: Int, rhs: Int) {
    clause(-lhs, rhs)
}

/**
 * [lhs] => ([base] <=> [rhs])
 */
fun Solver.implyIff(lhs: Int, base: Int, rhs: Int) {
    clause(-lhs, -base, rhs)
    clause(-lhs, base, -rhs)
}


/**
 * [lhs] => ([base] <=> AND([rhs]))
 */
fun Solver.implyIffAnd(lhs: Int, base: Int, rhs: Sequence<Int>) {
    clause(sequence {
        yield(-lhs)
        yield(base)
        for (x in rhs) {
            clause(-lhs, -base, x)
            yield(-x)
        }
    })
}

/**
 * [lhs] => ([base] <=> AND([rhs]))
 */
fun Solver.implyIffAnd(lhs: Int, base: Int, vararg rhs: Int) = implyIffAnd(lhs, base, rhs.asSequence())


/**
 * [lhs] => ([base] <=> OR([rhs]))
 */
fun Solver.implyIffOr(lhs: Int, base: Int, rhs: Sequence<Int>) {
    clause(sequence {
        yield(-lhs)
        yield(-base)
        for (x in rhs) {
            clause(-lhs, base, -x)
            yield(x)
        }
    })
}

/**
 * [lhs] => ([base] <=> OR([rhs]))
 */
fun Solver.implyIffOr(lhs: Int, base: Int, vararg rhs: Int) = implyIffOr(lhs, base, rhs.asSequence())


/**
 * [lhs] <=> [rhs]
 */
fun Solver.iff(lhs: Int, rhs: Int) {
    imply(lhs, rhs)
    imply(rhs, lhs)
}


/**
 * [lhs] <=> AND([rhs])
 */
fun Solver.iffAnd(lhs: Int, rhs: Sequence<Int>) {
    for (x in rhs)
        clause(-lhs, x)
    clause(sequenceOf(lhs) + rhs.map { -it })
}

/**
 * [lhs] <=> AND([rhs])
 */
fun Solver.iffAnd(lhs: Int, vararg rhs: Int) = iffAnd(lhs, rhs.asSequence())


/**
 * [lhs] <=> OR([rhs])
 */
fun Solver.iffOr(lhs: Int, rhs: Sequence<Int>) {
    for (x in rhs)
        clause(lhs, -x)
    clause(sequenceOf(-lhs) + rhs)
}

/**
 * [lhs] <=> OR([rhs])
 */
fun Solver.iffOr(lhs: Int, vararg rhs: Int) = iffOr(lhs, rhs.asSequence())
