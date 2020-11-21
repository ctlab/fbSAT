package ru.ifmo.fbsat.core.utils

interface Compound<out T> {
    val modular: ImmutableMultiArray<T>
}

@Deprecated("Inherit the CompoundImpl abstract class for this functionality")
val Compound<*>.M: Int
    get() = modular.toMultiArray().shape.single()

@Deprecated("Inherit the CompoundImpl abstract class for this functionality")
fun Compound<*>.stringify(): String = modular.values.toString()

@Suppress("PropertyName")
abstract class CompoundImpl<out T> : Compound<T> {
    open val M: Int by lazy { modular.toMultiArray().shape.single() }

    override fun toString(): String = modular.values.toString()
}

fun <T> Compound<T>.project(i: Int): T = modular.toMultiArray()[i]
fun <T> Iterable<Compound<T>>.project(i: Int): List<T> = map { it.project(i) }
