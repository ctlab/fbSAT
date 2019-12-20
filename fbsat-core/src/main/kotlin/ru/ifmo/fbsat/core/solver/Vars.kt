package ru.ifmo.fbsat.core.solver

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.utils.applyIfNotNull
import ru.ifmo.fbsat.core.utils.mapValues
import ru.ifmo.fbsat.core.utils.mapValuesToInt
import ru.ifmo.fbsat.core.utils.toBooleanArray

class BoolVar private constructor(
    private val backend: MultiArray<Int>
) : MultiArray<Int> by backend {
    fun convert(raw: RawAssignment): BooleanMultiArray {
        return BooleanMultiArray.create(shape) { index -> raw[get(*index)] }
    }

    companion object {
        @JvmStatic
        fun create(
            shape: IntArray,
            init: (IntArray) -> Int
        ): BoolVar = BoolVar(MultiArray.create(shape, init))

        @JvmStatic
        @JvmName("createVararg")
        fun create(
            vararg shape: Int,
            init: (IntArray) -> Int
        ): BoolVar = create(shape, init)
    }
}

class IntVar private constructor(
    private val backend: MultiArray<Domain<Int>>
) : MultiArray<Domain<Int>> by backend {
    @Suppress("ReplaceGetOrSet")
    fun domain(index: IntArray): Set<Int> = backend.get(*index).domain

    fun convert(raw: RawAssignment): IntMultiArray {
        return backend.mapValuesToInt { it.convert(raw) ?: error("So sad :c") }
    }

    companion object {
        @JvmStatic
        @JvmOverloads
        fun create(
            shape: IntArray,
            init: (IntArray, Int) -> Int,
            applyDomain: (Domain<Int>.() -> Unit)? = null,
            domain: (IntArray) -> Iterable<Int>
        ): IntVar =
            IntVar(MultiArray.create(shape) { index ->
                Domain(domain(index).associateWith { value -> init(index, value) })
                    .applyIfNotNull(applyDomain)
            })

        @JvmStatic
        @JvmOverloads
        @JvmName("createVararg")
        fun create(
            vararg shape: Int,
            init: (IntArray, Int) -> Int,
            applyDomain: (Domain<Int>.() -> Unit)? = null,
            domain: (IntArray) -> Iterable<Int>
        ): IntVar = create(shape, init, applyDomain, domain)
    }
}

class Var<T> private constructor(
    private val backend: MultiArray<Domain<T>>
) : MultiArray<Domain<T>> by backend {
    @Suppress("ReplaceGetOrSet")
    fun domain(index: IntArray): Set<T> = backend.get(*index).domain

    fun convert(raw: RawAssignment): MultiArray<T> {
        return backend.mapValues { it.convert(raw) ?: error("So sad :c") }
    }

    companion object {
        @JvmStatic
        @JvmOverloads
        fun <T> create(
            shape: IntArray,
            init: (IntArray, T) -> Int,
            applyDomain: (Domain<T>.() -> Unit)? = null,
            domain: (IntArray) -> Iterable<T>
        ): Var<T> =
            Var(MultiArray.create(shape) { index ->
                Domain(domain(index).associateWith { value -> init(index, value) })
                    .applyIfNotNull(applyDomain)
            })

        @JvmStatic
        @JvmOverloads
        @JvmName("createVararg")
        fun <T> create(
            vararg shape: Int,
            init: (IntArray, T) -> Int,
            applyDomain: (Domain<T>.() -> Unit)? = null,
            domain: (IntArray) -> Iterable<T>
        ): Var<T> = create(shape, init, applyDomain, domain)
    }
}

class Domain<T>(private val map: Map<T, Int>) {
    val domain: Set<T> = map.keys
    val values: Collection<Int> = map.values

    infix fun eq(value: T): Int = map.getValue(value)
    infix fun neq(value: T): Int = -eq(value)

    fun convert(raw: RawAssignment): T? {
        // TODO: ensure that at most one bit is true
        return map.entries.firstOrNull { raw[it.value] }?.key
    }
}

fun main() {
    val solver = Solver.mock()
    val init: (IntArray, Int) -> Int = { _, _ -> solver.newVariable() }
    val domainVar = IntVar.create(3, 2, init = init) { 1..4 }
    val raw = RawAssignment("001000100101100010001000".toBooleanArray())
    val data = domainVar.convert(raw)
    println("data = $data = ${data.toList()}")
}
