@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.core.solver

fun Solver.atLeastOne(literals: Sequence<Int>) {
    clause(literals)
}

fun Solver.atLeastOne(block: suspend SequenceScope<Int>.() -> Unit) {
    atLeastOne(sequence(block).constrainOnce())
}

fun Solver.atMostOne(literals: Sequence<Int>) {
    atMostOne(literals.toList())
}

fun Solver.atMostOne(block: suspend SequenceScope<Int>.() -> Unit) {
    atMostOne(sequence(block).constrainOnce())
}

fun Solver.exactlyOne(literals: Sequence<Int>) {
    exactlyOne(literals.toList())
}

fun Solver.exactlyOne(block: suspend SequenceScope<Int>.() -> Unit) {
    exactlyOne(sequence(block).constrainOnce())
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
fun Solver.implyOr(lhs: Int, rhs: Sequence<Int>) = implyOr(lhs, rhs.toList())

/**
 * [lhs] => OR([rhs])
 */
fun Solver.implyOr(lhs: Int, vararg rhs: Int) = implyOr(lhs, rhs.asIterable())

/**
 * [x1] => (x2 => AND([rhs])
 */
fun Solver.implyImplyAnd(x1: Int, x2: Int, rhs: Sequence<Int>) = implyImplyAnd(x1, x2, rhs.toList())

/**
 * [x1] => (x2 => AND([rhs])
 */
fun Solver.implyImplyAnd(x1: Int, x2: Int, vararg rhs: Int) = implyImplyAnd(x1, x2, rhs.asIterable())

/**
 * [x1] => (x2 => OR([rhs])
 */
fun Solver.implyImplyOr(x1: Int, x2: Int, rhs: Sequence<Int>) = implyImplyOr(x1, x2, rhs.toList())

/**
 * [x1] => (x2 => OR([rhs])
 */
fun Solver.implyImplyOr(x1: Int, x2: Int, vararg rhs: Int) = implyImplyOr(x1, x2, rhs.asIterable())

/**
 * [x1] => ([x2] => ([x3] <=> AND([xs]))
 */
fun Solver.implyImplyIffAnd(x1: Int, x2: Int, x3: Int, xs: Sequence<Int>) = implyImplyIffAnd(x1, x2, x3, xs.toList())

/**
 * [x1] => ([x2] => ([x3] <=> AND([xs]))
 */
fun Solver.implyImplyIffAnd(x1: Int, x2: Int, x3: Int, vararg xs: Int) = implyImplyIffAnd(x1, x2, x3, xs.asIterable())

/**
 * [x1] => ([x2] => ([x3] <=> OR([xs]))
 */
fun Solver.implyImplyIffOr(x1: Int, x2: Int, x3: Int, xs: Sequence<Int>) = implyImplyIffOr(x1, x2, x3, xs.toList())

/**
 * [x1] => ([x2] => ([x3] <=> OR([xs]))
 */
fun Solver.implyImplyIffOr(x1: Int, x2: Int, x3: Int, vararg xs: Int) = implyImplyIffOr(x1, x2, x3, xs.asIterable())

/**
 * [x1] => ([x2] <=> AND([xs]))
 */
fun Solver.implyIffAnd(x1: Int, x2: Int, xs: Sequence<Int>) = implyIffAnd(x1, x2, xs.toList())

/**
 * [x1] => ([x2] <=> AND([xs]))
 */
fun Solver.implyIffAnd(x1: Int, x2: Int, vararg xs: Int) = implyIffAnd(x1, x2, xs.asIterable())

/**
 * [x1] => ([x2] <=> OR([xs]))
 */
fun Solver.implyIffOr(x1: Int, x2: Int, xs: Sequence<Int>) = implyIffOr(x1, x2, xs.toList())

/**
 * [x1] => ([x2] <=> OR([xs]))
 */
fun Solver.implyIffOr(x1: Int, x2: Int, vararg xs: Int) = implyIffOr(x1, x2, xs.asIterable())

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
fun Solver.iffOr(lhs: Int, rhs: Sequence<Int>) = iffOr(lhs, rhs.toList())

/**
 * [lhs] <=> OR([rhs])
 */
fun Solver.iffOr(lhs: Int, vararg rhs: Int) = iffOr(lhs, rhs.asIterable())
