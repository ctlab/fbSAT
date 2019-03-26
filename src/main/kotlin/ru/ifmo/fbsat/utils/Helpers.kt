package ru.ifmo.fbsat.utils

import okio.BufferedSource
import okio.Source
import okio.buffer

fun String.toBooleanArray(): BooleanArray {
    return this.map {
        when (it) {
            '1' -> true
            '0' -> false
            else -> error("All characters in string '$it' must be '1' or '0'")
        }
    }.toBooleanArray()
}

fun BooleanArray.toBinaryString(): String {
    return this.joinToString("") { if (it) "1" else "0" }
}

fun randomBinaryString(length: Int): String {
    return (1..length).asSequence().map { "01".random() }.joinToString("")
}

fun <T> randomChoice(vararg choices: T): T {
    return choices.random()
}

object PnP {
    val inputEvents = listOf("REQ")
    val outputEvents = listOf("CNF")
    val inputNames = listOf("c1Home", "c1End", "c2Home", "c2End", "vcHome", "vcEnd", "pp1", "pp2", "pp3", "vac")
    val outputNames = listOf("c1Extend", "c1Retract", "c2Extend", "c2Retract", "vcExtend", "vacuum_on", "vacuum_off")
}

inline fun <T> Source.useLines(block: (Sequence<String>) -> T): T =
    buffer().use { block(it.lineSequence()) }

fun BufferedSource.lineSequence(): Sequence<String> =
    sequence<String> { while (true) yield(readUtf8Line() ?: break) }.constrainOnce()
