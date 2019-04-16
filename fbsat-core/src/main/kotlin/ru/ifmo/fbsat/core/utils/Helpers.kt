package ru.ifmo.fbsat.core.utils

import okio.BufferedSource
import okio.Source
import okio.buffer
import okio.gzip
import okio.sink
import okio.source
import ru.ifmo.multiarray.BooleanMultiArray
import ru.ifmo.multiarray.IntMultiArray
import java.io.File
import kotlin.random.Random

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

fun ClosedRange<Double>.random(): Double {
    return random(Random)
}

fun ClosedRange<Double>.random(random: Random): Double {
    return start + random.nextDouble() * (endInclusive - start)
}

/**
 * Pick-and-Place manipulator input events.
 */
val inputEventsPnP = listOf("REQ")
/**
 * Pick-and-Place manipulator output events.
 */
val outputEventsPnP = listOf("CNF")
/**
 * Pick-and-Place manipulator input variables names.
 */
val inputNamesPnP = listOf("c1Home", "c1End", "c2Home", "c2End", "vcHome", "vcEnd", "pp1", "pp2", "pp3", "vac")
/**
 * Pick-and-Place manipulator output variables names.
 */
val outputNamesPnP = listOf("c1Extend", "c1Retract", "c2Extend", "c2Retract", "vcExtend", "vacuum_on", "vacuum_off")

inline fun <T> Source.useLines(block: (Sequence<String>) -> T): T =
    buffer().use { block(it.lineSequence()) }

fun BufferedSource.lineSequence(): Sequence<String> =
    sequence<String> { while (true) yield(readUtf8Line() ?: break) }.constrainOnce()

fun copyFile(source: File, destination: File) {
    // Note: destination folder existence must be ensured externally!
    source.source().use { a ->
        destination.sink().buffer().use { b ->
            b.writeAll(a)
        }
    }
}

fun IntMultiArray.Companion.empty() = IntMultiArray(intArrayOf()) { 0 }
fun BooleanMultiArray.Companion.empty() = BooleanMultiArray(intArrayOf()) { false }

/**
 * Measures the [block] execution time and returns a [Pair](result, runningTime).
 * @param[block] code to execute.
 * @return [Pair] of [block] execution result and running time (in seconds).
 */
inline fun <T> timeIt(block: () -> T): Pair<T, Double> {
    val timeStart = System.currentTimeMillis()
    val result = block()
    return result to (System.currentTimeMillis() - timeStart) / 1000.0
}

fun File.sourceAutoGzip(): Source =
    if (path.endsWith(".gz", ignoreCase = true))
        source().gzip()
    else
        source()
