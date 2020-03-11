package ru.ifmo.fbsat.core.task.single.extforest

fun p2ck(p: Int, K: Int): Pair<Int, Int> {
    val c = (p - 1) / K + 1
    val k = (p - 1) % K + 1
    return Pair(c, k)
}

fun ck2p(c: Int, k: Int, K: Int): Int {
    return (c - 1) * K + (k - 1) + 1
}
