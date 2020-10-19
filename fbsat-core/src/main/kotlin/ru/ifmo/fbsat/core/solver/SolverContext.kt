package ru.ifmo.fbsat.core.solver

import com.github.lipen.multiarray.MultiArray
import kotlin.properties.ReadOnlyProperty
import kotlin.reflect.KProperty

class SolverContext internal constructor(
    val solver: Solver,
    val map: MutableMap<String, Any> = mutableMapOf()
) {
    // Usage:
    //   val context: SolverContext = solver.newContext()
    //
    //   val x: T = context["name"]
    //      - calls get<T>("name")
    //      - delegates to Map<String, T>::get("name")
    //
    //   context["name"] = x
    //      - calls set("name")
    //      - delegates to MutableMap<String, Any>::set("name", x)
    //
    //   val x: T by context
    //      - calls getValue<T>(...)
    //      - delegates to get<T>("x")
    //
    //   val x by context(42)
    //      - calls invoke<Int>(42)
    //      - provides a delegate property which stores the specified value in the context
    //      - delegates to set("x", 42)
    //
    //   val x: IntMultiArray by context.convertIntVarArray(raw)
    //      - provides a delegate property which converts IntVarArray to IntMultiArray
    //      - calls getValue(...) of a delegate property

    @Suppress("UNCHECKED_CAST")
    operator fun <T> get(key: String): T = map.getValue(key) as T

    operator fun set(key: String, value: Any) {
        map[key] = value
    }

    operator fun <T> getValue(thisRef: Any?, property: KProperty<*>): T = get(property.name)

    operator fun setValue(thisRef: Any?, property: KProperty<*>, value: Any) {
        set(property.name, value)
    }

    // @Deprecated("To be removed", level = DeprecationLevel.ERROR)
    operator fun <T : Any> invoke(value: T): ContextProvider<T> = ContextProvider(value)
    // inline operator fun <T : Any> invoke(init: Solver.() -> T): ContextProvider<T> = invoke(solver.init())

    inner class ContextProvider<T : Any>(val value: T) {
        operator fun provideDelegate(thisRef: Any?, property: KProperty<*>): ReadOnlyProperty<Any?, T> {
            this@SolverContext.setValue(thisRef, property, value)
            return object : ReadOnlyProperty<Any?, T> {
                // It is more correct to return `this@SolverContext.getValue(thisRef, property)`,
                // because we already saved it in the context,
                // but just `value` is simpler and faster.
                override fun getValue(thisRef: Any?, property: KProperty<*>): T = value
            }
        }
    }
}

fun Solver.declareModularContext(M: Int): MultiArray<SolverContext> {
    val modularContext by context(MultiArray.create(M) { newContext() })
    return modularContext
}

fun SolverContext.mutateIntVarArray(
    name: String,
    vararg shape: Int,
    init: (orig: IntVarArray, index: IntArray) -> IntVar
) {
    val orig: IntVarArray = this[name]
    this[name] = IntVarArray.create(shape) { init(orig, it) }
}

fun SolverContext.mutateBoolVarArray(
    name: String,
    vararg shape: Int,
    init: (orig: BoolVarArray, index: IntArray) -> Literal
) {
    val orig: BoolVarArray = this[name]
    this[name] = BoolVarArray.create(shape) { init(orig, it) }
}
