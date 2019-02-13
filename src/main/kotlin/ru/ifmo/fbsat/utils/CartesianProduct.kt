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

fun main() {
    val lists: Sequence<List<Any>> = sequenceOf(
        listOf("A", "B"),
        listOf(1, 2, 3, 4),
        listOf("!", "?")
    )
    println("lists = ${lists.toList()}")
    val cp = lists.cartesianProduct.toList()
    println("cartesianProduct(lists) = ${cp.map { it.joinToString("") }.toList()}")
    println("cp.size = ${cp.size} (should be ${lists.map { it.size }.reduce(Int::times)})")
}
