package ru.ifmo.fbsat.core.utils

interface Compound<out T> {
    val modular: ImmutableMultiArray<T>
}

val Compound<*>.M: Int get() = modular.toMultiArray().shape.single()

fun Compound<*>.stringify(): String = modular.values.toString()
