package ru.ifmo.fbsat.core.utils

interface Compound<out T> {
    val modular: ImmutableMultiArray<T>
}

val <T> Compound<T>.M: Int get() = modular.shape.single()
