package ru.ifmo.fbsat.core.task.extra.circuit

import kotlin.math.pow

operator fun <T> List<T>.get(indices: IntRange): List<T> = slice(indices)
operator fun <T> List<T>.get(indices: Iterable<Int>): List<T> = slice(indices)

operator fun String.get(indices: IntRange): String = slice(indices)
operator fun String.get(indices: Iterable<Int>): String = slice(indices)

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
