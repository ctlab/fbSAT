package ru.ifmo.fbsat.core.utils

import com.github.lipen.multiarray.MultiArray

interface ImmutableMultiArray<out T> : MultiArray<@UnsafeVariance T>

@Deprecated("Something is wrong if you are using it!")
fun <T> ImmutableMultiArray<T>.toMultiArray(): MultiArray<T> = this

@Deprecated("Something is wrong if you are using it!")
fun <T> MultiArray<T>.toImmutable(): ImmutableMultiArray<T> =
    object : ImmutableMultiArray<T>, MultiArray<T> by this@toImmutable {}
