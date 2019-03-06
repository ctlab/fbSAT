package ru.ifmo.fbsat.utils

val <T> Sequence<Iterable<T>>.cartesianProduct: Sequence<List<T>>
    get() = this
        .map { it.asSequence() }
        .fold(sequenceOf(sequenceOf<T>())) { acc, outer ->
            acc.flatMap { sub -> outer.map { elem -> sub + elem } }
        }
        .map { it.toList() }

val <T> Iterable<Iterable<T>>.cartesianProduct: Sequence<List<T>>
    get() = this.asSequence().cartesianProduct

fun <A, B, T> product(
    xs1: Iterable<A>,
    xs2: Iterable<B>,
    block: (A, B) -> T
) = sequence {
    for (x1 in xs1) for (x2 in xs2)
        yield(block(x1, x2))
}

fun <A, B, C, T> product(
    xs1: Iterable<A>,
    xs2: Iterable<B>,
    xs3: Iterable<C>,
    block: (A, B, C) -> T
) = sequence {
    for (x1 in xs1) for (x2 in xs2) for (x3 in xs3)
        yield(block(x1, x2, x3))
}

fun <A, B, C, D, T> product(
    xs1: Iterable<A>,
    xs2: Iterable<B>,
    xs3: Iterable<C>,
    xs4: Iterable<D>,
    block: (A, B, C, D) -> T
) = sequence {
    for (x1 in xs1) for (x2 in xs2) for (x3 in xs3) for (x4 in xs4)
        yield(block(x1, x2, x3, x4))
}

fun <A, B, C, D, E, T> product(
    xs1: Iterable<A>,
    xs2: Iterable<B>,
    xs3: Iterable<C>,
    xs4: Iterable<D>,
    xs5: Iterable<E>,
    block: (A, B, C, D, E) -> T
) = sequence {
    for (x1 in xs1) for (x2 in xs2) for (x3 in xs3) for (x4 in xs4) for (x5 in xs5)
        yield(block(x1, x2, x3, x4, x5))
}

fun main() {
    val lists: Sequence<List<Any>> = sequenceOf(
        listOf("A", "B"),
        listOf(1, 2, 3, 4),
        listOf("!", "?")
    )
    println("lists = ${lists.toList()}")
    val cp = lists.cartesianProduct.toList()
    println("cartesianProduct(lists) = ${cp.map { it.joinToString("") }.toList()}")
    require(cp.size == 16) { "cp.size should be 16" }

    product("AB".asIterable(), 1..3, "!?".asIterable()) { a, b, c ->
        println("abc = $a$b$c")
    }
}
