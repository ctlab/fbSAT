package ru.ifmo.fbsat.core.solver

fun Solver.atLeastOne(literals: List<Int>) {
    clause(literals)
}

fun Solver.atLeastOne(literals: Sequence<Int>) {
    clause(literals)
}

fun Solver.atLeastOne(block: suspend SequenceScope<Int>.() -> Unit) {
    atLeastOne(sequence(block).constrainOnce())
}

fun Solver.atMostOne(literals: List<Int>) {
    for (i in literals.indices)
        for (j in (i + 1) until literals.size)
            clause(-literals[i], -literals[j])
}

fun Solver.atMostOne(literals: Sequence<Int>) {
    atMostOne(literals.toList())
}

fun Solver.atMostOne(block: suspend SequenceScope<Int>.() -> Unit) {
    atMostOne(sequence(block).constrainOnce())
}

fun Solver.exactlyOne(literals: List<Int>) {
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
fun Solver.implyAnd(lhs: Int, rhs: List<Int>) {
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
fun Solver.implyAnd(lhs: Int, vararg rhs: Int) = implyAnd(lhs, rhs.asList())

/**
 * [lhs] => OR([rhs])
 */
fun Solver.implyOr(lhs: Int, rhs: List<Int>) {
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
fun Solver.implyOr(lhs: Int, vararg rhs: Int) = implyOr(lhs, rhs.asList())

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
fun Solver.implyIffAnd(lhs: Int, base: Int, rhs: List<Int>) {
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
fun Solver.implyIffAnd(lhs: Int, base: Int, vararg rhs: Int) = implyIffAnd(lhs, base, rhs.asList())

/**
 * [lhs] => ([base] <=> OR([rhs]))
 */
fun Solver.implyIffOr(lhs: Int, base: Int, rhs: List<Int>) {
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
fun Solver.implyIffOr(lhs: Int, base: Int, vararg rhs: Int) = implyIffOr(lhs, base, rhs.asList())

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
fun Solver.iffAnd(lhs: Int, rhs: List<Int>) {
    for (x in rhs)
        clause(-lhs, x)
    clause(sequenceOf(lhs) + rhs.asSequence().map { -it })
}

/**
 * [lhs] <=> AND([rhs])
 */
fun Solver.iffAnd(lhs: Int, rhs: Sequence<Int>) = iffAnd(lhs, rhs.toList())

/**
 * [lhs] <=> AND([rhs])
 */
fun Solver.iffAnd(lhs: Int, vararg rhs: Int) = iffAnd(lhs, rhs.asList())

/**
 * [lhs] <=> OR([rhs])
 */
fun Solver.iffOr(lhs: Int, rhs: List<Int>) {
    for (x in rhs)
        clause(lhs, -x)
    clause(sequenceOf(-lhs) + rhs)
}

/**
 * [lhs] <=> OR([rhs])
 */
fun Solver.iffOr(lhs: Int, rhs: Sequence<Int>) = iffOr(lhs, rhs.toList())

/**
 * [lhs] <=> OR([rhs])
 */
fun Solver.iffOr(lhs: Int, vararg rhs: Int) = iffOr(lhs, rhs.asList())
