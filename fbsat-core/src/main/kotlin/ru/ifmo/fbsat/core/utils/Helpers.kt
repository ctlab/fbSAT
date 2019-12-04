package ru.ifmo.fbsat.core.utils

import com.github.lipen.multiarray.IntMultiArray
import com.soywiz.klock.DateTime
import com.soywiz.klock.DateTimeTz
import okio.BufferedSource
import okio.Source
import okio.buffer
import okio.gzip
import okio.sink
import okio.source
import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.scenario.ScenarioTreeInterface
import java.io.File
import kotlin.random.Random

fun String.toBooleanArray(): BooleanArray {
    return BooleanArray(length) { i ->
        when (this[i]) {
            '1' -> true
            '0' -> false
            else -> error("All characters in string '$this' must be '1' or '0'")
        }
    }
}

fun String.toBooleanList(): List<Boolean> {
    return map {
        when (it) {
            '1' -> true
            '0' -> false
            else -> error("All characters in string '$it' must be '1' or '0'")
        }
    }
}

fun BooleanArray.toBinaryString(): String {
    return joinToString("") { if (it) "1" else "0" }
}

fun List<Boolean>.toBinaryString(): String {
    return joinToString("") { if (it) "1" else "0" }
}

@JvmName("toBinaryStringNullable")
fun List<Boolean?>.toBinaryString(): String {
    return joinToString("") {
        when (it) {
            true -> "1"
            false -> "0"
            null -> "x"
        }
    }
}

fun randomBinaryString(length: Int): String {
    return (1..length).asSequence().map { "01".random() }.joinToString("")
}

fun <T> randomChoice(vararg choices: T): T {
    return choices.random()
}

fun randomBooleanList(size: Int): List<Boolean> {
    return List(size) { randomChoice(true, false) }
}

fun ClosedRange<Double>.random(random: Random): Double {
    return start + random.nextDouble() * (endInclusive - start)
}

fun ClosedRange<Double>.random(): Double {
    return random(Random)
}

/**
 * Pick-and-Place manipulator input events.
 */
val inputEventsPnP = listOf("REQ").map(::InputEvent)
/**
 * Pick-and-Place manipulator output events.
 */
val outputEventsPnP = listOf("CNF").map(::OutputEvent)
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

/**
 * Measures the [block] execution time and returns a [Pair](result, runningTime).
 * @param[block] code to execute.
 * @return [Pair] of [block] execution result and running time (in seconds).
 */
inline fun <T> timeIt(block: () -> T): Pair<T, Double> {
    val timeStart = DateTime.now()
    val result = block()
    return result to secondsSince(timeStart)
}

fun secondsSince(timeStart: DateTime): Double = (DateTime.now() - timeStart).seconds

fun secondsSince(timeStart: DateTimeTz): Double = (DateTime.nowLocal() - timeStart).seconds

fun File.sourceAutoGzip(): Source =
    if (extension == "gz")
        source().gzip()
    else
        source()

/**
 * Forcibly get value from map.
 */
fun <K, V, T> Map<K, V>.getForce(key: K): T {
    @Suppress("UNCHECKED_CAST")
    return this[key] as T
}

val <T> T.exhaustive: T
    get() = this

inline fun <reified T> mutableListOfNulls(size: Int): MutableList<T?> = MutableList(size) { null }

fun <T> Iterable<T>.withIndex(start: Int): Iterable<Pair<Int, T>> {
    return asSequence().mapIndexed { index, value -> (start + index) to value }.asIterable()
}

fun <T> Iterable<T>.firstIndexed(predicate: (Int, T) -> Boolean): T =
    withIndex().first { (index, value) -> predicate(index, value) }.value

fun Iterable<Boolean>.all(): Boolean = all { it }

fun <T> cartesianProduct(iterables: Iterable<Iterable<T>>): Sequence<List<T>> =
    if (iterables.count() == 0) emptySequence()
    else iterables.fold(sequenceOf(listOf())) { acc, iterable ->
        acc.flatMap { list -> iterable.asSequence().map { element -> list + element } }
    }

fun <T> Iterable<T>.pairs(): Sequence<Pair<T, T>> = sequence {
    for ((i, v) in withIndex()) {
        yieldAll(drop(i + 1).map { v to it })
    }
}

fun <T> Iterable<T>.pairsWithReplacement(): Sequence<Pair<T, T>> = sequence {
    for ((i, v) in withIndex()) {
        yieldAll(drop(i).map { v to it })
    }
}

fun boolToSign(b: Boolean): Int = if (b) 1 else -1

fun algorithmChoice(
    tree: ScenarioTreeInterface,
    v: Int,
    c: Int,
    z: Int,
    algorithmTop: IntMultiArray,
    algorithmBot: IntMultiArray
): Int {
    val p = tree.parent(v)
    val oldValue = tree.outputValue(p, z)
    val newValue = tree.outputValue(v, z)
    return when (val values = oldValue to newValue) {
        true to true -> algorithmTop[c, z]
        true to false -> -algorithmTop[c, z]
        false to true -> algorithmBot[c, z]
        false to false -> -algorithmBot[c, z]
        else -> error("Weird combination of values: $values")
    }
}

fun <T> Iterable<T>.joinPadded(length: Int, separator: String = " "): String =
    joinToString(separator) { it.toString().padStart(length) }
