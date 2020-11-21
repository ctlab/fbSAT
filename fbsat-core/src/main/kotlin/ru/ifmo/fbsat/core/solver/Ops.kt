package ru.ifmo.fbsat.core.solver

import ru.ifmo.fbsat.core.utils.pairs

/** `AtLeastOne`([literals]) */
fun Solver.atLeastOne(literals: Iterable<Literal>) {
    clause(literals)
}

/** `AtMostOne`([literals]) */
fun Solver.atMostOne(literals: Iterable<Literal>) {
    for ((a, b) in literals.pairs())
        imply(a, -b)
}

/** `ExactlyOne`([literals]) */
fun Solver.exactlyOne(literals: Iterable<Literal>) {
    val pool = literals.toList()
    atLeastOne(pool)
    atMostOne(pool)
}

/** [lhs] => [rhs] */
fun Solver.imply(lhs: Literal, rhs: Literal) {
    clause(-lhs, rhs)
}

/** [lhs] => `AND`([rhs]) */
fun Solver.implyAnd(lhs: Literal, rhs: Iterable<Literal>) {
    for (x in rhs)
        imply(lhs, x)
}

/** [lhs] => `OR`([rhs]) */
fun Solver.implyOr(lhs: Literal, rhs: Iterable<Literal>) {
    clause {
        yield(-lhs)
        for (x in rhs)
            yield(x)
    }
}

/** [x1] => ([x2] => [x3]) */
fun Solver.implyImply(x1: Literal, x2: Literal, x3: Literal) {
    clause(-x1, -x2, x3)
}

/** [x1] => ([x2] <=> [x3]) */
fun Solver.implyIff(x1: Literal, x2: Literal, x3: Literal) {
    implyImply(x1, x2, x3)
    implyImply(x1, x3, x2)
}

/** [lhs] => `ITE`([cond], [a], [b]) */
fun Solver.implyIte(lhs: Literal, cond: Literal, a: Literal, b: Literal) {
    implyImply(lhs, cond, a)
    implyImply(lhs, -cond, b)
}

/** [x1] => ([x2] => `AND`([xs]) */
fun Solver.implyImplyAnd(x1: Literal, x2: Literal, xs: Iterable<Literal>) {
    for (x in xs)
        implyImply(x1, x2, x)
}

/** [x1] => ([x2] => `OR`([xs]) */
fun Solver.implyImplyOr(x1: Literal, x2: Literal, xs: Iterable<Literal>) {
    clause {
        yield(-x1)
        yield(-x2)
        for (x in xs)
            yield(x)
    }
}

/** [x1] => ([x2] => ([x3] => [x4])) */
fun Solver.implyImplyImply(x1: Literal, x2: Literal, x3: Literal, x4: Literal) {
    clause(-x1, -x2, -x3, x4)
}

/** [x1] => ([x2] => ([x3] => `AND`([xs]))) */
fun Solver.implyImplyImplyAnd(x1: Literal, x2: Literal, x3: Literal, xs: Iterable<Literal>) {
    for (x in xs)
        implyImplyImply(x1, x2, x3, x)
}

/** [x1] => ([x2] => ([x3] => `OR`([xs]))) */
fun Solver.implyImplyImplyOr(x1: Literal, x2: Literal, x3: Literal, xs: Iterable<Literal>) {
    clause {
        yield(-x1)
        yield(-x2)
        yield(-x3)
        for (x in xs)
            yield(x)
    }
}

/** [x1] => ([x2] => ([x3] => ([base] <=> `ITE`([cond], [a], [b])))) */
fun Solver.implyImplyImplyIffIte(
    x1: Literal,
    x2: Literal,
    x3: Literal,
    base: Literal,
    cond: Literal,
    a: Literal,
    b: Literal,
) {
    clause(-x1, -x2, -x3, base, -cond, -a)
    clause(-x1, -x2, -x3, -base, -cond, a)
    clause(-x1, -x2, -x3, base, cond, -b)
    clause(-x1, -x2, -x3, -base, cond, b)
}

/** [x1] => ([x2] => ([x3] <=> [x4])) */
fun Solver.implyImplyIff(x1: Literal, x2: Literal, x3: Literal, x4: Literal) {
    implyImplyImply(x1, x2, x3, x4)
    implyImplyImply(x1, x2, x4, x3)
}

/** [x1] => ([x2] => ([x3] <=> `AND`([xs])) */
fun Solver.implyImplyIffAnd(x1: Literal, x2: Literal, x3: Literal, xs: Iterable<Literal>) {
    clause {
        yield(-x1)
        yield(-x2)
        yield(x3)
        for (x in xs) {
            implyImplyImply(x1, x2, x3, x)
            yield(-x)
        }
    }
}

/** [x1] => ([x2] => ([x3] <=> `OR`([xs]))) */
fun Solver.implyImplyIffOr(x1: Literal, x2: Literal, x3: Literal, xs: Iterable<Literal>) {
    clause {
        yield(-x1)
        yield(-x2)
        yield(-x3)
        for (x in xs) {
            implyImplyImply(x1, x2, x, x3)
            yield(x)
        }
    }
}

/** [x1] => ([x2] <=> `AND`([xs])) */
fun Solver.implyIffAnd(x1: Literal, x2: Literal, xs: Iterable<Literal>) {
    clause {
        yield(-x1)
        yield(x2)
        for (x in xs) {
            implyImply(x1, x2, x)
            yield(-x)
        }
    }
}

/** [x1] => ([x2] <=> `OR`([xs])) */
fun Solver.implyIffOr(x1: Literal, x2: Literal, xs: Iterable<Literal>) {
    clause {
        yield(-x1)
        yield(-x2)
        for (x in xs) {
            implyImply(x1, x, x2)
            yield(x)
        }
    }
}

/** [x1] => ([x2] <=> `ITE`([cond], [a], [b]) */
fun Solver.implyIffIte(x1: Literal, x2: Literal, cond: Literal, a: Literal, b: Literal) {
    clause(-x1, x2, -cond, -a)
    clause(-x1, -x2, -cond, a)
    clause(-x1, x2, cond, -b)
    clause(-x1, -x2, cond, b)
}

/** [lhs] <=> [rhs] */
fun Solver.iff(lhs: Literal, rhs: Literal) {
    imply(lhs, rhs)
    imply(rhs, lhs)
}

/** [lhs] <=> `AND`([rhs]) */
fun Solver.iffAnd(lhs: Literal, rhs: Iterable<Literal>) {
    clause {
        yield(lhs)
        for (x in rhs) {
            imply(lhs, x)
            yield(-x)
        }
    }
}

/** [lhs] <=> `OR`([rhs]) */
fun Solver.iffOr(lhs: Literal, rhs: Iterable<Literal>) {
    clause {
        yield(-lhs)
        for (x in rhs) {
            imply(x, lhs)
            yield(x)
        }
    }
}

/** [lhs] <=> ([x1] => [x2]) */
fun Solver.iffImply(lhs: Literal, x1: Literal, x2: Literal) {
    implyImply(lhs, x1, x2)
    clause(lhs, x1)
    clause(lhs, -x2)
}

/** [lhs] <=> `ITE`([cond], [a], [b]) */
fun Solver.iffIte(lhs: Literal, cond: Literal, a: Literal, b: Literal) {
    // implyIte(lhs, cond, a, b)
    // implyImply(cond, a, lhs)
    // implyImply(-cond, b, lhs)
    clause(lhs, -cond, -a)
    clause(-lhs, -cond, a)
    clause(lhs, cond, -b)
    clause(-lhs, cond, b)
}
