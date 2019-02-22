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

    // Note: one-based
    override fun get(vararg index: Int): T {
        return buffer[strides.offset1(index)]
    }

    // Note: one-based
    override fun set(vararg index: Int, value: T) {
        buffer[strides.offset1(index)] = value
    }

    override fun toString(): String {
        return "MultiArray(values = $values)"
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
        return buffer[strides.offset1(index)]
    }

    override operator fun set(vararg index: Int, value: Int) {
        buffer[strides.offset1(index)] = value
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
        return buffer[strides.offset1(index)]
    }

    override operator fun set(vararg index: Int, value: Boolean) {
        buffer[strides.offset1(index)] = value
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
            for (dim in shape) {
                cur *= dim
                yield(cur)
            }
        }.toList()
    }

    fun offset0(index: IntArray): Int {
        return index.asSequence().mapIndexed { i, value ->
            if (value !in 1..shape[i]) throw IndexOutOfBoundsException("Index $value for dimension ${i + 1} is out of shape bounds (${1..shape[i]})")
            value * strides[i]
        }.sum()
    }

    fun offset1(index: IntArray): Int {
        return index.asSequence().mapIndexed { i, value ->
            if (value !in 1..shape[i]) throw IndexOutOfBoundsException("Index $value for dimension ${i + 1} is out of shape bounds (${1..shape[i]})")
            (value - 1) * strides[i]
        }.sum()
    }

    fun index0(offset: Int): IntArray {
        val res = IntArray(shape.size)
        var current = offset
        var strideIndex = strides.size - 2
        while (strideIndex >= 0) {
            res[strideIndex] = (current / strides[strideIndex])
            current %= strides[strideIndex]
            strideIndex--
        }
        return res
    }

    fun index1(offset: Int): IntArray {
        val res = IntArray(shape.size)
        var current = offset
        var strideIndex = strides.size - 2
        while (strideIndex >= 0) {
            res[strideIndex] = (current / strides[strideIndex]) + 1
            current %= strides[strideIndex]
            strideIndex--
        }
        return res
    }
}
