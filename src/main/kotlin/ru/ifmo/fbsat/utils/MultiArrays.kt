package ru.ifmo.fbsat.utils

/**
 * Multi-dimensional *one-based* array inspired by [kmath][https://github.com/altavir/kmath] library.
 */
class MultiArray<T>(shape: IntArray, init: (IntArray) -> T) {
    private val strides = Strides(shape)
    private val buffer = MutableList(shape.reduce(Int::times)) { init(strides.index1(it)) }

    val shape: IntArray
        get() = strides.shape

    val values: Collection<T>
        get() = buffer

    // Note: one-based
    operator fun get(vararg index: Int): T {
        return buffer[strides.offset1(index)]
    }

    // Note: one-based
    operator fun set(vararg index: Int, value: T) {
        buffer[strides.offset1(index)] = value
    }

    // Note: one-based
    fun <R> mapIndexed(transform: (IntArray, T) -> R): List<R> {
        return buffer.mapIndexed { offset, value -> transform(strides.index1(offset), value) }
    }

    companion object {
        fun <T> new(vararg shape: Int, init: (IntArray) -> T) = MultiArray(shape, init)
    }
}

class IntMultiArray(shape: IntArray, init: (IntArray) -> Int) {
    private val strides = Strides(shape)
    private val buffer = IntArray(shape.reduce(Int::times)) { init(strides.index1(it)) }

    val shape: IntArray
        get() = strides.shape

    val values: Collection<Int>
        get() = buffer.toList()

    operator fun get(vararg index: Int): Int {
        // return buffer[strides.offset(index)]
        return buffer[strides.offset1(index)]
    }

    operator fun set(vararg index: Int, value: Int) {
        // buffer[strides.offset(index)] = value
        buffer[strides.offset1(index)] = value
    }

    fun <R> mapIndexed(transform: (IntArray, Int) -> R): List<R> {
        return buffer.mapIndexed { offset, value -> transform(strides.index1(offset), value) }
    }

    companion object {
        fun new(vararg shape: Int, init: (IntArray) -> Int = { 0 }) = IntMultiArray(shape, init)
    }
}

class BooleanMultiArray(shape: IntArray, init: (IntArray) -> Boolean) {
    private val strides = Strides(shape)
    private val buffer = BooleanArray(shape.reduce(Int::times)) { init(strides.index1(it)) }

    val shape: IntArray
        get() = strides.shape

    val values: Collection<Boolean>
        get() = buffer.toList()

    operator fun get(vararg index: Int): Boolean {
        return buffer[strides.offset1(index)]
    }

    operator fun set(vararg index: Int, value: Boolean) {
        buffer[strides.offset1(index)] = value
    }

    fun <R> mapIndexed(transform: (IntArray, Boolean) -> R): List<R> {
        return buffer.mapIndexed { offset, value -> transform(strides.index1(offset), value) }
    }

    companion object {
        fun new(vararg shape: Int, init: (IntArray) -> Boolean = { false }) = BooleanMultiArray(shape, init)
    }
}

private class Strides(val shape: IntArray) {
    private val strides: List<Int> by lazy {
        // shape.asIterable().reductions(1, Int::times).toList()
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
            require(value in 0 until shape[i]) { "Index $value is out of shape bounds (0, ${shape[i] - 1})" }
            value * strides[i]
        }.sum()
    }

    fun offset1(index: IntArray): Int {
        return index.asSequence().mapIndexed { i, value ->
            require(value in 1..shape[i]) { "Index $value is out of shape bounds (1, ${shape[i]})" }
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
