package ru.ifmo.fbsat.core.utils

import com.github.lipen.multiarray.MultiArray

interface Compound<out T> {
    val modular: MultiArray<T>
}

@Deprecated("Inherit the CompoundImpl abstract class for this functionality", level = DeprecationLevel.ERROR)
val Compound<*>.M: Int
    get() = modular.shape.single()

@Deprecated("Inherit the CompoundImpl abstract class for this functionality", level = DeprecationLevel.ERROR)
fun Compound<*>.stringify(): String = modular.values.toString()

abstract class CompoundImpl<out T> : Compound<T> {
    @Suppress("PropertyName")
    open val M: Int by lazy { modular.shape.single() }

    override fun toString(): String = modular.values.toString()
}

fun <T> Compound<T>.project(i: Int): T = modular[i]
fun <T> Iterable<Compound<T>>.project(i: Int): List<T> = map { it.project(i) }
