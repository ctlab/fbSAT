package ru.ifmo.fbsat.core.solver

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray

class RawAssignment(private val data: BooleanArray) {
    operator fun get(v: Int): Boolean = when (v) {
        Solver.trueVariable -> true
        Solver.falseVariable -> false
        else -> data[v - 1]
    }

    fun BoolVar.convert(): BooleanMultiArray = convert(this@RawAssignment)
    fun IntVar.convert(): IntMultiArray = convert(this@RawAssignment)
    fun <T> Var<T>.convert(): MultiArray<T> = convert(this@RawAssignment)
}
