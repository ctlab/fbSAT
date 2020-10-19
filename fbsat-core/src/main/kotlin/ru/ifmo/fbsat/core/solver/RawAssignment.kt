package ru.ifmo.fbsat.core.solver

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import com.github.lipen.multiarray.mapToInt
import kotlin.math.absoluteValue
import kotlin.properties.ReadOnlyProperty
import kotlin.reflect.KProperty

interface RawAssignment {
    operator fun get(v: Literal): Boolean
}

class RawAssignment0(private val data: BooleanArray) : RawAssignment {
    override operator fun get(v: Literal): Boolean = when (v) {
        Solver.trueLiteral -> true
        Solver.falseLiteral -> false
        else -> data[v.absoluteValue - 1] xor (v < 0)
    }

    override fun toString(): String {
        return "RawAssignment0(data = ${data.toList()})"
    }
}

class RawAssignment1(private val data: BooleanArray) : RawAssignment {
    override operator fun get(v: Literal): Boolean = when (v) {
        Solver.trueLiteral -> true
        Solver.falseLiteral -> false
        else -> data[v.absoluteValue] xor (v < 0)
    }

    override fun toString(): String {
        return "RawAssignment1(data = ${data.toList()})"
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

@Suppress("PublicApiImplicitType")
fun <T : Any> SolverContext.convertDomainVar(raw: RawAssignment) =
    object : ReadOnlyProperty<Any?, T?> {
        override operator fun getValue(thisRef: Any?, property: KProperty<*>): T? =
            this@convertDomainVar.getValue<DomainVar<T>>(thisRef, property).convert(raw)
    }

@Suppress("PublicApiImplicitType")
fun SolverContext.convertIntVar(raw: RawAssignment) =
    object : ReadOnlyProperty<Any?, Int?> {
        override operator fun getValue(thisRef: Any?, property: KProperty<*>): Int? =
            this@convertIntVar.getValue<IntVar>(thisRef, property).convert(raw)
    }

@Suppress("PublicApiImplicitType")
fun SolverContext.convertLiteral(raw: RawAssignment) =
    object : ReadOnlyProperty<Any?, Boolean> {
        override operator fun getValue(thisRef: Any?, property: KProperty<*>): Boolean =
            raw[this@convertLiteral.getValue(thisRef, property)]
    }

@Suppress("PublicApiImplicitType")
inline fun <reified T> SolverContext.convertDomainVarArray(raw: RawAssignment) =
    object : ReadOnlyProperty<Any?, MultiArray<T>> {
        override operator fun getValue(thisRef: Any?, property: KProperty<*>): MultiArray<T> =
            this@convertDomainVarArray.getValue<DomainVarArray<T>>(thisRef, property).convert(raw)
    }

@Suppress("PublicApiImplicitType")
fun SolverContext.convertIntVarArray(raw: RawAssignment) =
    object : ReadOnlyProperty<Any?, IntMultiArray> {
        override operator fun getValue(thisRef: Any?, property: KProperty<*>): IntMultiArray =
            this@convertIntVarArray.getValue<IntVarArray>(thisRef, property).convert(raw)
    }

@Suppress("PublicApiImplicitType")
fun SolverContext.convertBoolVarArray(raw: RawAssignment) =
    object : ReadOnlyProperty<Any?, BooleanMultiArray> {
        override operator fun getValue(thisRef: Any?, property: KProperty<*>): BooleanMultiArray =
            this@convertBoolVarArray.getValue<BoolVarArray>(thisRef, property).convert(raw)
    }
