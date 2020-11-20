package ru.ifmo.fbsat.core.solver

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import com.github.lipen.multiarray.mapToBoolean
import com.github.lipen.multiarray.mapToInt
import kotlin.math.absoluteValue

interface Model {
    operator fun get(v: Literal): Boolean
}

class Model0(private val data: BooleanArray) : Model {
    override operator fun get(v: Literal): Boolean = when (v) {
        Solver.trueLiteral -> true
        Solver.falseLiteral -> false
        else -> data[v.absoluteValue - 1] xor (v < 0)
    }

    override fun toString(): String {
        return "Model0(data = ${data.asList()})"
    }
}

class Model1(private val data: BooleanArray) : Model {
    override operator fun get(v: Literal): Boolean = when (v) {
        Solver.trueLiteral -> true
        Solver.falseLiteral -> false
        else -> data[v.absoluteValue] xor (v < 0)
    }

    override fun toString(): String {
        return "Model1(data = ${data.asList()})"
    }
}

fun BoolVarArray.convert(model: Model): BooleanMultiArray =
    mapToBoolean { model[it] }

fun IntVarArray.convert(model: Model): IntMultiArray =
    mapToInt { it.convert(model) ?: error("So sad :c") }

inline fun <reified T> DomainVarArray<T>.convert(model: Model): MultiArray<T> =
    map { it.convert(model) ?: error("So sad :c") }

@JvmName("multiArrayBoolVarArrayConvert")
fun MultiArray<BoolVarArray>.convert(model: Model): MultiArray<BooleanMultiArray> =
    map { it.convert(model) }

@JvmName("multiArrayIntVarArrayConvert")
fun MultiArray<IntVarArray>.convert(model: Model): MultiArray<IntMultiArray> =
    map { it.convert(model) }

@JvmName("multiArrayDomainVarArrayConvert")
inline fun <reified T> MultiArray<DomainVarArray<T>>.convert(model: Model): MultiArray<MultiArray<T>> =
    map { it.convert(model) }

fun Context.convertLiteral(name: String, model: Model): Boolean =
    model[get(name)]

fun <T : Any> Context.convertDomainVar(name: String, model: Model): T? =
    get<DomainVar<T>>(name).convert(model)

fun Context.convertIntVar(name: String, model: Model): Int? =
    get<IntVar>(name).convert(model)

inline fun <reified T> Context.convertDomainVarArray(name: String, model: Model): MultiArray<T> =
    get<DomainVarArray<T>>(name).convert(model)

fun Context.convertIntVarArray(name: String, model: Model): IntMultiArray =
    get<IntVarArray>(name).convert(model)

fun Context.convertBoolVarArray(name: String, model: Model): BooleanMultiArray =
    get<BoolVarArray>(name).convert(model)
