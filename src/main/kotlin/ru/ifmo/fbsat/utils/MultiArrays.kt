package ru.ifmo.fbsat.utils

/**
 * Multi-dimensional array inspired by [kmath][https://github.com/altavir/kmath].
 *
 * @param[T] element type.
 */
interface MultiArray<T> {
    val shape: IntArray
    val values: Collection<T>

    operator fun get(vararg index: Int): T
    operator fun set(vararg index: Int, value: T)

    companion object {
        fun <T> new(vararg shape: Int, init: (IntArray) -> T): MultiArray<T> = DefaultMultiArray(shape, init)
        fun new(vararg shape: Int, init: (IntArray) -> Int = { 0 }) = IntMultiArray(shape, init)
        fun new(vararg shape: Int, init: (IntArray) -> Boolean = { false }) = BooleanMultiArray(shape, init)
    }
}

/**
 * One-based multi-dimensional array.
 *
 * @param[T] element type.
 */
private class DefaultMultiArray<T>(
    override val shape: IntArray,
    init: (IntArray) -> T
) : MultiArray<T> {
    private val strides = Strides(shape)
    private val buffer =
        MutableList(if (shape.isNotEmpty()) shape.reduce(Int::times) else 0) {
            init(strides.index1(it))
        }

    override val values: Collection<T> = buffer

    override fun get(vararg index: Int): T {
        validate(index)
        return buffer[strides.offset1(index)]
    }

    override fun set(vararg index: Int, value: T) {
        validate(index)
        buffer[strides.offset1(index)] = value
    }

    override fun toString(): String {
        return "MultiArray(shape = ${shape.asList()})"
    }
}

class IntMultiArray(
    override val shape: IntArray,
    init: (IntArray) -> Int
) : MultiArray<Int> {
    private val strides = Strides(shape)
    private val buffer =
        IntArray(if (shape.isNotEmpty()) shape.reduce(Int::times) else 0) {
            init(strides.index1(it))
        }

    override val values: Collection<Int>
        get() = buffer.toList()

    override operator fun get(vararg index: Int): Int {
        validate(index)
        return buffer[strides.offset1(index)]
    }

    override operator fun set(vararg index: Int, value: Int) {
        validate(index)
        buffer[strides.offset1(index)] = value
    }

    override fun toString(): String {
        return "IntMultiArray(shape = ${shape.asList()})"
    }

    companion object {
        fun new(vararg shape: Int, init: (IntArray) -> Int = { 0 }) = IntMultiArray(shape, init)
    }
}

class BooleanMultiArray(
    override val shape: IntArray,
    init: (IntArray) -> Boolean
) : MultiArray<Boolean> {
    private val strides = Strides(shape)
    private val buffer =
        BooleanArray(if (shape.isNotEmpty()) shape.reduce(Int::times) else 0) {
            init(strides.index1(it))
        }

    override val values: Collection<Boolean>
        get() = buffer.toList()

    override operator fun get(vararg index: Int): Boolean {
        validate(index)
        return buffer[strides.offset1(index)]
    }

    override operator fun set(vararg index: Int, value: Boolean) {
        validate(index)
        buffer[strides.offset1(index)] = value
    }

    override fun toString(): String {
        return "BooleanMultiArray(shape = ${shape.asList()})"
    }

    companion object {
        fun new(vararg shape: Int, init: (IntArray) -> Boolean = { false }) = BooleanMultiArray(shape, init)
    }
}

private class Strides(val shape: IntArray) {
    private val strides: List<Int> by lazy {
        sequence {
            yield(1)
            var cur = 1
            for (i in (shape.size - 1) downTo 1) {
                cur *= shape[i]
                yield(cur)
            }
        }.toList().reversed()
    }

    fun offset0(index0: IntArray): Int =
        index0.asSequence().zip(strides.asSequence()) { i, s -> i * s }.sum()

    fun offset1(index1: IntArray): Int =
        index1.asSequence().zip(strides.asSequence()) { i, s -> (i - 1) * s }.sum()

    fun index0(offset: Int): IntArray {
        val result = IntArray(shape.size)
        var current = offset
        for ((i, s) in strides.withIndex()) {
            result[i] = current / s // 0-based
            current %= s
            if (current == 0) break
        }
        return result
    }

    fun index1(offset: Int): IntArray {
        val result = IntArray(shape.size) { 1 }
        var current = offset
        for ((i, s) in strides.withIndex()) {
            result[i] = current / s + 1 // 1-based
            current %= s
            if (current == 0) break
        }
        return result
    }
}

private fun <T> MultiArray<T>.validate(index: IntArray) {
    require(index.size == shape.size) {
        "Invalid number of dimensions passed: index.size = ${index.size}, shape.size = ${shape.size}"
    }
    for (i in index.indices) {
        val ix = index[i]
        val domain = 1..shape[i]
        if (ix !in domain)
            throw IndexOutOfBoundsException("Index $ix (${i + 1}-th) out of bounds ($domain)")
    }
}
