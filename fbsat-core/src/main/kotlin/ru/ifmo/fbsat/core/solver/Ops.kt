package ru.ifmo.fbsat.core.solver

import ru.ifmo.fbsat.core.utils.pairs

fun Solver.atLeastOne(literals: Iterable<Int>) {
    clause(literals)
}

fun Solver.atLeastOne(literals: Sequence<Int>) {
    clause(literals)
}

fun Solver.atLeastOne(block: suspend SequenceScope<Int>.() -> Unit) {
    atLeastOne(sequence(block).constrainOnce())
}

fun Solver.atMostOne(literals: Iterable<Int>) {
    for ((a, b) in literals.pairs())
        clause(-a, -b)
}

fun Solver.atMostOne(literals: Sequence<Int>) {
    atMostOne(literals.toList())
}

fun Solver.atMostOne(block: suspend SequenceScope<Int>.() -> Unit) {
    atMostOne(sequence(block).constrainOnce())
}

fun Solver.exactlyOne(literals: Iterable<Int>) {
    atLeastOne(literals)
    atMostOne(literals)
}

fun Solver.exactlyOne(literals: Sequence<Int>) {
    exactlyOne(literals.toList())
}

fun Solver.exactlyOne(block: suspend SequenceScope<Int>.() -> Unit) {
    exactlyOne(sequence(block).constrainOnce())
}

/**
 * [lhs] => [rhs]
 */
fun Solver.imply(lhs: Int, rhs: Int) {
    clause(-lhs, rhs)
}

/**
 * [lhs] => AND([rhs])
 */
fun Solver.implyAnd(lhs: Int, rhs: Iterable<Int>) {
    for (x in rhs)
        imply(lhs, x)
}

/**
 * [lhs] => AND([rhs])
 */
fun Solver.implyAnd(lhs: Int, rhs: Sequence<Int>) = implyAnd(lhs, rhs.toList())

/**
 * [lhs] => AND([rhs])
 */
fun Solver.implyAnd(lhs: Int, vararg rhs: Int) = implyAnd(lhs, rhs.asIterable())

/**
 * [lhs] => OR([rhs])
 */
fun Solver.implyOr(lhs: Int, rhs: Iterable<Int>) {
    clause {
        yield(-lhs)
        for (x in rhs)
            yield(x)
    }
}

/**
 * [lhs] => OR([rhs])
 */
fun Solver.implyOr(lhs: Int, rhs: Sequence<Int>) = implyOr(lhs, rhs.toList())

/**
 * [lhs] => OR([rhs])
 */
fun Solver.implyOr(lhs: Int, vararg rhs: Int) = implyOr(lhs, rhs.asIterable())

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
fun Solver.implyIffAnd(lhs: Int, base: Int, rhs: Iterable<Int>) {
    clause {
        yield(-lhs)
        yield(base)
        for (x in rhs) {
            clause(-lhs, -base, x)
            yield(-x)
        }
    }
}

/**
 * [lhs] => ([base] <=> AND([rhs]))
 */
fun Solver.implyIffAnd(lhs: Int, base: Int, rhs: Sequence<Int>) = implyIffAnd(lhs, base, rhs.toList())

/**
 * [lhs] => ([base] <=> AND([rhs]))
 */
fun Solver.implyIffAnd(lhs: Int, base: Int, vararg rhs: Int) = implyIffAnd(lhs, base, rhs.asIterable())

/**
 * [lhs] => ([base] <=> OR([rhs]))
 */
fun Solver.implyIffOr(lhs: Int, base: Int, rhs: Iterable<Int>) {
    clause {
        yield(-lhs)
        yield(-base)
        for (x in rhs) {
            clause(-lhs, base, -x)
            yield(x)
        }
    }
}

/**
 * [lhs] => ([base] <=> OR([rhs]))
 */
fun Solver.implyIffOr(lhs: Int, base: Int, rhs: Sequence<Int>) = implyIffOr(lhs, base, rhs.toList())

/**
 * [lhs] => ([base] <=> OR([rhs]))
 */
fun Solver.implyIffOr(lhs: Int, base: Int, vararg rhs: Int) = implyIffOr(lhs, base, rhs.asIterable())

/**
 * [lhs] <= [rhs]
 */
fun Solver.impliedBy(lhs: Int, rhs: Int) {
    imply(rhs, lhs)
}

/**
 * [lhs] <= AND([rhs])
 */
fun Solver.impliedByAnd(lhs: Int, rhs: Iterable<Int>) {
    clause {
        yield(lhs)
        for (x in rhs)
            yield(-x)
    }
}

/**
 * [lhs] <= AND([rhs])
 */
fun Solver.impliedByAnd(lhs: Int, rhs: Sequence<Int>) = impliedByAnd(lhs, rhs.toList())

/**
 * [lhs] <= AND([rhs])
 */
fun Solver.impliedByAnd(lhs: Int, vararg rhs: Int) = impliedByAnd(lhs, rhs.asIterable())

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
fun Solver.iffAnd(lhs: Int, rhs: Iterable<Int>) {
    clause {
        yield(lhs)
        for (x in rhs) {
            clause(-lhs, x)
            yield(-x)
        }
    }
}

/**
 * [lhs] <=> AND([rhs])
 */
fun Solver.iffAnd(lhs: Int, rhs: Sequence<Int>) = iffAnd(lhs, rhs.toList())

/**
 * [lhs] <=> AND([rhs])
 */
fun Solver.iffAnd(lhs: Int, vararg rhs: Int) = iffAnd(lhs, rhs.asIterable())

/**
 * [lhs] <=> OR([rhs])
 */
fun Solver.iffOr(lhs: Int, rhs: Iterable<Int>) {
    clause {
        yield(-lhs)
        for (x in rhs) {
            clause(lhs, -x)
            yield(x)
        }
    }
}

/**
 * [lhs] <=> OR([rhs])
 */
fun Solver.iffOr(lhs: Int, rhs: Sequence<Int>) = iffOr(lhs, rhs.toList())

/**
 * [lhs] <=> OR([rhs])
 */
fun Solver.iffOr(lhs: Int, vararg rhs: Int) = iffOr(lhs, rhs.asIterable())

/**
 * [x1] => ([x2] => (x3 <=> x4))
 */
fun Solver.implyImplyIff(x1: Int, x2: Int, x3: Int, x4: Int) {
    clause(-x1, -x2, -x3, x4)
    clause(-x1, -x2, x3, -x4)
}

/**
 * [aux] <=> ([lhs] => [rhs])
 */
fun Solver.iffImply(aux: Int, lhs: Int, rhs: Int) {
    clause(-aux, -lhs, rhs)
    clause(aux, lhs)
    clause(aux, -rhs)
}
