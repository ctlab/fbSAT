package ru.ifmo.fbsat.utils

interface MultiDomainArray<K : Any, V : Any> {
    val domains: Iterable<Iterable<K>>
    val values: Collection<V>

    operator fun get(vararg index: K): V

    operator fun set(vararg index: K, value: V)

    companion object {
        fun <K : Any, V : Any> new(
            domains: Iterable<Iterable<K>>,
            init: (List<K>) -> V
        ): MultiDomainArray<K, V> = DefaultMultiDomainArray(domains, init)

        fun <K : Any, V : Any> new(
            vararg domains: Iterable<K>,
            init: (List<K>) -> V
        ): MultiDomainArray<K, V> = new(domains.asIterable(), init)
    }
}

private class DefaultMultiDomainArray<K : Any, V : Any>(
    override val domains: Iterable<Iterable<K>>,
    init: (List<K>) -> V
) : MultiDomainArray<K, V> {
    private val storage: MutableMap<List<K>, V> = domains.cartesianProduct.associateWith(init).toMutableMap()
    private val types: List<Class<*>> = domains.map { it.first().javaClass }

    override val values: Collection<V> = storage.values

    private fun Array<out K>.checkTypes() {
        val indexTypes = this.map { it.javaClass }
        require(indexTypes == types) {
            "Types mismatch (index: ${indexTypes.map { it.simpleName }}, multi-array: ${types.map { it.simpleName }})"
        }
    }

    override operator fun get(vararg index: K): V {
        return storage[index.asList()]
            ?: run {
                index.checkTypes()
                throw IndexOutOfBoundsException("Index ${index.asList()} is out of bounds")
            }
    }

    override operator fun set(vararg index: K, value: V) {
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

    // val solver = DefaultSolver("")

    // val colors = (1..10).map { Color(it) }
    // val transitions = (1..5).map { TransitionNumber(it) }
    // val safe: SatArray = solver.newArray(colors, transitions)
    // println("safe = $safe")
    // val c = Color(5)
    // val t = TransitionNumber(3)
    // println("safe[c, t] = ${safe[c, t]}")
    // println("safe[c, c] = ${safe[c, c]}") // throws
}

private enum class MyType {
    A, B, C, D
}
