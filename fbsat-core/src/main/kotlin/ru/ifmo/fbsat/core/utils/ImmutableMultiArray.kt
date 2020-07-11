package ru.ifmo.fbsat.core.utils

import com.github.lipen.multiarray.MultiArray

interface ImmutableMultiArray<out T> : MultiArray<@UnsafeVariance T>

@Deprecated("Something is wrong if you are using it!")
fun <T> ImmutableMultiArray<T>.toMultiArray(): MultiArray<T> = this

@Deprecated("Something is wrong if you are using it!")
fun <T> MultiArray<T>.toImmutable(): ImmutableMultiArray<T> =
    object : ImmutableMultiArray<T> {
        private val delegate = this@toImmutable

        override val dims: Int
            get() = delegate.dims
        override val domains: List<IntRange>
            get() = delegate.domains
        override val indices: Sequence<IntArray>
            get() = delegate.indices
        override val shape: IntArray
            get() = delegate.shape
        override val values: List<T>
            get() = delegate.values

        override fun get(i: Int): T = delegate[i]

        override fun get(i: Int, j: Int): T = delegate[i, j]

        override fun get(i: Int, j: Int, k: Int): T = delegate[i, j, k]

        override fun getAt(index: IntArray): T = delegate.getAt(index)

        override fun set(i: Int, value: T) {
            error("This MultiArray is immutable!")
        }

        override fun set(i: Int, j: Int, value: T) {
            error("This MultiArray is immutable!")
        }

        override fun set(i: Int, j: Int, k: Int, value: T) {
            error("This MultiArray is immutable!")
        }

        override fun setAt(index: IntArray, value: T) {
            error("This MultiArray is immutable!")
        }
    }
