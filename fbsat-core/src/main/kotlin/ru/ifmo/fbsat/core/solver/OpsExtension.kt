@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.core.solver

/** AtLeastOne([literals]) */
fun Solver.atLeastOne(literals: Sequence<Literal>) =
    atLeastOne(literals.asIterable())

/** AtLeastOne(literals) */
fun Solver.atLeastOne(block: suspend SequenceScope<Literal>.() -> Unit) =
    atLeastOne(sequence(block).constrainOnce())

/** AtMostOne([literals]) */
fun Solver.atMostOne(literals: Sequence<Literal>) =
    atMostOne(literals.asIterable())

/** AtMostOne(literals) */
fun Solver.atMostOne(block: suspend SequenceScope<Literal>.() -> Unit) =
    atMostOne(sequence(block).constrainOnce())

/** ExactlyOne([literals]) */
fun Solver.exactlyOne(literals: Sequence<Literal>) =
    exactlyOne(literals.asIterable())

/** ExactlyOne(literals) */
fun Solver.exactlyOne(block: suspend SequenceScope<Literal>.() -> Unit) =
    exactlyOne(sequence(block).constrainOnce())

/** [lhs] => AND([rhs]) */
fun Solver.implyAnd(lhs: Literal, rhs: Sequence<Literal>) =
    implyAnd(lhs, rhs.asIterable())

/** [lhs] => AND([rhs]) */
fun Solver.implyAnd(lhs: Literal, vararg rhs: Literal) =
    implyAnd(lhs, rhs.asIterable())

/** [lhs] => OR([rhs]) */
fun Solver.implyOr(lhs: Literal, rhs: Sequence<Literal>) =
    implyOr(lhs, rhs.asIterable())

/** [lhs] => OR([rhs]) */
fun Solver.implyOr(lhs: Literal, vararg rhs: Literal) =
    implyOr(lhs, rhs.asIterable())

/** [x1] => (x2 => AND([rhs]) */
fun Solver.implyImplyAnd(x1: Literal, x2: Literal, rhs: Sequence<Literal>) =
    implyImplyAnd(x1, x2, rhs.asIterable())

/** [x1] => (x2 => AND([rhs]) */
fun Solver.implyImplyAnd(x1: Literal, x2: Literal, vararg rhs: Literal) =
    implyImplyAnd(x1, x2, rhs.asIterable())

/** [x1] => (x2 => OR([rhs]) */
fun Solver.implyImplyOr(x1: Literal, x2: Literal, rhs: Sequence<Literal>) =
    implyImplyOr(x1, x2, rhs.asIterable())

/** [x1] => (x2 => OR([rhs]) */
fun Solver.implyImplyOr(x1: Literal, x2: Literal, vararg rhs: Literal) =
    implyImplyOr(x1, x2, rhs.asIterable())

/**  [x1] => ([x2] => ([x3] => AND([xs])) */
fun Solver.implyImplyImplyAnd(x1: Literal, x2: Literal, x3: Literal, xs: Sequence<Literal>) =
    implyImplyImplyAnd(x1, x2, x3, xs.asIterable())

/** [x1] => ([x2] => ([x3] => AND([xs])) */
fun Solver.implyImplyImplyAnd(x1: Literal, x2: Literal, x3: Literal, vararg xs: Literal) =
    implyImplyImplyAnd(x1, x2, x3, xs.asIterable())

/** [x1] => ([x2] => ([x3] => OR([xs])) */
fun Solver.implyImplyImplyOr(x1: Literal, x2: Literal, x3: Literal, xs: Sequence<Literal>) =
    implyImplyImplyOr(x1, x2, x3, xs.asIterable())

/** [x1] => ([x2] => ([x3] => OR([xs])) */
fun Solver.implyImplyImplyOr(x1: Literal, x2: Literal, x3: Literal, vararg xs: Literal) =
    implyImplyImplyOr(x1, x2, x3, xs.asIterable())

/**  [x1] => ([x2] => ([x3] <=> AND([xs])) */
fun Solver.implyImplyIffAnd(x1: Literal, x2: Literal, x3: Literal, xs: Sequence<Literal>) =
    implyImplyIffAnd(x1, x2, x3, xs.asIterable())

/** [x1] => ([x2] => ([x3] <=> AND([xs])) */
fun Solver.implyImplyIffAnd(x1: Literal, x2: Literal, x3: Literal, vararg xs: Literal) =
    implyImplyIffAnd(x1, x2, x3, xs.asIterable())

/** [x1] => ([x2] => ([x3] <=> OR([xs])) */
fun Solver.implyImplyIffOr(x1: Literal, x2: Literal, x3: Literal, xs: Sequence<Literal>) =
    implyImplyIffOr(x1, x2, x3, xs.asIterable())

/** [x1] => ([x2] => ([x3] <=> OR([xs])) */
fun Solver.implyImplyIffOr(x1: Literal, x2: Literal, x3: Literal, vararg xs: Literal) =
    implyImplyIffOr(x1, x2, x3, xs.asIterable())

/** [x1] => ([x2] <=> AND([xs])) */
fun Solver.implyIffAnd(x1: Literal, x2: Literal, xs: Sequence<Literal>) =
    implyIffAnd(x1, x2, xs.asIterable())

/** [x1] => ([x2] <=> AND([xs])) */
fun Solver.implyIffAnd(x1: Literal, x2: Literal, vararg xs: Literal) =
    implyIffAnd(x1, x2, xs.asIterable())

/** [x1] => ([x2] <=> OR([xs])) */
fun Solver.implyIffOr(x1: Literal, x2: Literal, xs: Sequence<Literal>) =
    implyIffOr(x1, x2, xs.asIterable())

/** [x1] => ([x2] <=> OR([xs])) */
fun Solver.implyIffOr(x1: Literal, x2: Literal, vararg xs: Literal) =
    implyIffOr(x1, x2, xs.asIterable())

/** [lhs] <=> AND([rhs]) */
fun Solver.iffAnd(lhs: Literal, rhs: Sequence<Literal>) =
    iffAnd(lhs, rhs.asIterable())

/** [lhs] <=> AND([rhs]) */
fun Solver.iffAnd(lhs: Literal, vararg rhs: Literal) =
    iffAnd(lhs, rhs.asIterable())

/** [lhs] <=> OR([rhs]) */
fun Solver.iffOr(lhs: Literal, rhs: Sequence<Literal>) =
    iffOr(lhs, rhs.asIterable())

/** [lhs] <=> OR([rhs]) */
fun Solver.iffOr(lhs: Literal, vararg rhs: Literal) =
    iffOr(lhs, rhs.asIterable())
