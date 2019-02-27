package ru.ifmo.fbsat.utils

fun String.toBooleanArray(): BooleanArray {
    return this.map {
        when (it) {
            '1' -> true
            '0' -> false
            else -> error("All characters in string '$it' must be '1' or '0'")
        }
    }.toBooleanArray()
}

fun BooleanArray.toBooleanString(): String {
    return this.joinToString("") { if (it) "1" else "0" }
}
