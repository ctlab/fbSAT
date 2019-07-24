package ru.ifmo.fbsat.core.utils

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.solver.Solver

internal interface TheAssignment {
    fun BooleanArray.convertBooleanArray(
        variable: IntMultiArray,
        vararg shape: Int
    ) = BooleanMultiArray.create(shape) { index ->
        @Suppress("ReplaceGetOrSet")
        when (val v = variable.get(*index)) {
            Solver.trueVariable -> true
            Solver.falseVariable -> false
            else -> this[v - 1]
        }
    }

    fun BooleanArray.convertIntArray(
        variable: IntMultiArray,
        vararg shape: Int,
        domain: Iterable<Int>, // FIXME: why not Collection<Int>?
        onAbsence: (index: IntArray) -> Int /*= { error("variable[index = $it] is undefined") }*/
    ) = IntMultiArray.create(shape) { index ->
        domain.firstOrNull { last ->
            @Suppress("ReplaceGetOrSet")
            when (val v = variable.get(*index, last)) {
                Solver.trueVariable -> true
                Solver.falseVariable -> false
                else -> this[v - 1]
            }
        } ?: onAbsence(index)
    }
}
