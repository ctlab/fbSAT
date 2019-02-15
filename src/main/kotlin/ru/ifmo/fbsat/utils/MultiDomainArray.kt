package ru.ifmo.fbsat.utils

import ru.ifmo.fbsat.solver.DefaultSolver

interface MultiDomainArray<T : Any, R : Any> {
    val domains: Iterable<Iterable<R>>
    val values: Collection<T>

    operator fun get(vararg index: R): T

    operator fun set(vararg index: R, value: T)

    companion object {
        fun <T : Any, R : Any> new(
            vararg domains: Iterable<R>,
            init: (List<R>) -> T
        ): MultiDomainArray<T, R> = DefaultMultiDomainArray(domains.asIterable(), init)
    }
}

private class DefaultMultiDomainArray<T : Any, R : Any>(
    override val domains: Iterable<Iterable<R>>,
    init: (List<R>) -> T
) : MultiDomainArray<T, R> {
    private val storage: MutableMap<List<R>, T>
    private val types: List<Class<*>>

    override val values: Collection<T>

    init {
        storage = domains.cartesianProduct.map { index -> index to init(index) }.toMap().toMutableMap()
        types = domains.map { it.first().javaClass }
        values = storage.values
    }

    private fun Array<out R>.checkTypes() {
        val indexTypes = this.map { it.javaClass }
        if (indexTypes != types)
            throw IllegalArgumentException("Types mismatch (index: ${indexTypes.map { it.simpleName }}, multi-array: ${types.map { it.simpleName }})")
    }

    override operator fun get(vararg index: R): T {
        return storage[index.asList()]
            ?: run {
                index.checkTypes()
                throw IndexOutOfBoundsException("Index ${index.asList()} is out of bounds")
            }
    }

    override operator fun set(vararg index: R, value: T) {
        storage.replace(index.asList(), value)
            ?: run {
                index.checkTypes()
                throw IndexOutOfBoundsException("Index ${index.asList()} is out of bounds")
            }
    }

    override fun toString(): String {
        return "MultiDomainArray(values = $values)"
    }
}


fun main() {
    val x = MultiDomainArray.new(
        1..5,
        MyType.values().asIterable()
    ) { (i, t) ->
        "val($i, $t)"
    }
    println("x = $x")
    println("x[4, B] = ${x[4, MyType.B]}")
    x[3, MyType.D] = "42!"
    println("x[3, D] = ${x[3, MyType.D]}")
    // println("x[3, lol] = ${x[3, "lol"]}")

    val solver = DefaultSolver("")

    val colors = (1..10).map { Color(it) }
    val transitions = (1..5).map { TransitionIndex(it) }
    val safe = solver.newDomainArray(colors, transitions)
    // val safe = SolverVariableMultiArray.new(
    //     solver,
    //     colors,
    //     (1..5).map { TransitionIndex(it) }
    // )
    println("safe = $safe")
    val c = Color(5)
    val t = TransitionIndex(3)
    println("safe[c, t] = ${safe[c, t]}")
    println("safe[c, c] = ${safe[c, c]}")  // throws
}

private enum class MyType {
    A, B, C, D
}

private sealed class Variable {
    abstract val v: Int
}

private data class Color(override val v: Int) : Variable()
private data class TransitionIndex(override val v: Int) : Variable()
