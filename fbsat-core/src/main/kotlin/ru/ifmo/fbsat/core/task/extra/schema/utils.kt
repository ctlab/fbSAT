package ru.ifmo.fbsat.core.task.extra.schema

import com.github.lipen.multiarray.MultiArray
import kotlin.math.pow

fun Int.pow(n: Int): Int =
    if (this == 2) 1 shl n
    else this.toDouble().pow(n).toInt()

fun Long.pow(n: Int): Long =
    if (this == 2L) 1L shl n
    else this.toDouble().pow(n).toLong()

fun Boolean.toInt(): Int = if (this) 1 else 0

fun valuesToTruthTable(values: List<Boolean?>, X: Int): Map<Input, Boolean> {
    val tt: MutableMap<Input, Boolean> = mutableMapOf()
    for ((i, b) in values.withIndex()) {
        if (b != null) {
            tt[Input(i, X)] = b
        }
    }
    return tt
}

@Suppress("LocalVariableName")
fun String.toTruthTable(X: Int): Map<Input, Boolean> {
    val values = map {
        when (it) {
            '0' -> false
            '1' -> true
            'x', '-' -> null
            else -> error("Bad char '$it'")
        }
    }
    return valuesToTruthTable(values, X)
}

@Suppress("LocalVariableName")
fun ttToBinaryString(tt: Map<Input, Boolean>): String {
    val X = tt.keys.first().values.size
    return (0 until 2.pow(X)).joinToString("") { f -> tt[Input(f, X)]?.toInt()?.toString() ?: "x" }
}

fun <T> MultiArray<T>.toList1(): List<T> {
    require(shape.size == 1)
    val (a) = shape
    return List(a) { i -> this[i + 1] }
}

fun <T> MultiArray<T>.toList2(): List<List<T>> {
    require(shape.size == 2)
    val (a, b) = shape
    return List(a) { i -> List(b) { j -> this[i + 1, j + 1] } }
}

fun <T> MultiArray<T>.toList3(): List<List<List<T>>> {
    require(shape.size == 3)
    val (a, b, c) = shape
    return List(a) { i -> List(b) { j -> List(c) { k -> this[i + 1, j + 1, k + 1] } } }
}
