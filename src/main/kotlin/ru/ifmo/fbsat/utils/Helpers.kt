package ru.ifmo.fbsat.utils

fun <T, R> Iterable<T>.reductions(
    initial: R,
    operation: (acc: R, T) -> R
): Sequence<R> = sequence {
    var cur = initial
    forEach {
        cur = operation(cur, it)
        yield(cur)
    }
}

// fun <T> Array<T>.mapInPlace(transform: (T) -> T) {
//    for (i in this.indices) {
//        this[i] = transform(this[i])
//    }
// }

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
