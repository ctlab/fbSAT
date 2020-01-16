package ru.ifmo.fbsat.core.solver

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import com.github.lipen.multiarray.mapToInt

class RawAssignment(private val data: BooleanArray) {
    operator fun get(v: Literal): Boolean = when (v) {
        Solver.trueVariable -> true
        Solver.falseVariable -> false
        else -> data[v - 1]
    }
}

fun BoolVarArray.convert(raw: RawAssignment): BooleanMultiArray =
    BooleanMultiArray.create(shape) { index -> raw[getAt(index)] }

fun IntVarArray.convert(raw: RawAssignment): IntMultiArray =
    mapToInt { it.convert(raw) ?: error("So sad :c") }

inline fun <reified T> DomainVarArray<T>.convert(raw: RawAssignment): MultiArray<T> =
    map { it.convert(raw) ?: error("So sad :c") }

@JvmName("multiArrayBoolVarArrayConvert")
fun MultiArray<BoolVarArray>.convert(raw: RawAssignment): MultiArray<BooleanMultiArray> =
    map { it.convert(raw) }

@JvmName("multiArrayIntVarArrayConvert")
fun MultiArray<IntVarArray>.convert(raw: RawAssignment): MultiArray<IntMultiArray> =
    map { it.convert(raw) }

@JvmName("multiArrayDomainVarArrayConvert")
inline fun <reified T> MultiArray<DomainVarArray<T>>.convert(raw: RawAssignment): MultiArray<MultiArray<T>> =
    map { it.convert(raw) }
