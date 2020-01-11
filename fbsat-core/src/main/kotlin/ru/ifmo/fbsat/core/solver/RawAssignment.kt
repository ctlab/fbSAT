package ru.ifmo.fbsat.core.solver

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.utils.mapValues
import ru.ifmo.fbsat.core.utils.mapValuesToInt

class RawAssignment(private val data: BooleanArray) {
    operator fun get(v: Literal): Boolean = when (v) {
        Solver.trueVariable -> true
        Solver.falseVariable -> false
        else -> data[v - 1]
    }
}

fun BoolVarArray.convert(raw: RawAssignment): BooleanMultiArray =
    BooleanMultiArray.create(shape) { index -> raw[getBy(index)] }

fun IntVarArray.convert(raw: RawAssignment): IntMultiArray =
    mapValuesToInt { it.convert(raw) ?: error("So sad :c") }

fun <T> VarArray<T>.convert(raw: RawAssignment): MultiArray<T> =
    mapValues { it.convert(raw) ?: error("So sad :c") }

@JvmName("multiArrayBoolVarArrayConvert")
fun MultiArray<BoolVarArray>.convert(raw: RawAssignment): MultiArray<BooleanMultiArray> =
    mapValues { it.convert(raw) }

@JvmName("multiArrayIntVarArrayConvert")
fun MultiArray<IntVarArray>.convert(raw: RawAssignment): MultiArray<IntMultiArray> =
    mapValues { it.convert(raw) }

@JvmName("multiArrayVarArrayConvert")
fun <T> MultiArray<VarArray<T>>.convert(raw: RawAssignment): MultiArray<MultiArray<T>> =
    mapValues { it.convert(raw) }
