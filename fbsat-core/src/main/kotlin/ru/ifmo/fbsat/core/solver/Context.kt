package ru.ifmo.fbsat.core.solver

import kotlin.properties.PropertyDelegateProvider
import kotlin.properties.ReadWriteProperty
import kotlin.reflect.KProperty

/**
 * Context.
 *
 * **Examples:**
 *
 * ```
 * // Create Context
 * val context: Context = newContext()
 * ```
 *
 * ```
 * // Retrieve value from context
 * val x: T = context["name"]
 * ```
 *
 * ```
 * // Put value into context
 * context["name"] = 42
 * val x = context("name", 42)
 * val x = context("name") { 42 }  // eager eval
 * ```
 *
 * ```
 * // Bind property (both val and var are possible)
 * var x: T by context.bind()
 * ```
 *
 * ```
 * // Bind caching property with initial value
 * val x by context.bindCaching(42)
 * ```
 */
class Context internal constructor(
    private val map: MutableMap<String, Any> = mutableMapOf(),
) {
    operator fun <T> get(key: String): T {
        @Suppress("UNCHECKED_CAST")
        return map.getValue(key) as T
    }

    operator fun set(key: String, value: Any) {
        map[key] = value
    }

    operator fun <T : Any> invoke(name: String, value: T): T = value.also { this[name] = it }

    inline operator fun <T : Any> invoke(name: String, init: () -> T): T = invoke(name, init())

    fun <T : Any> bind(): ReadWriteProperty<Any?, T> =
        object : ReadWriteProperty<Any?, T> {
            override fun getValue(thisRef: Any?, property: KProperty<*>): T {
                return get(property.name)
            }

            override fun setValue(thisRef: Any?, property: KProperty<*>, value: T) {
                set(property.name, value)
            }
        }

    fun <T : Any> bind(value: T): PropertyDelegateProvider<Any?, ReadWriteProperty<Any?, T>> =
        PropertyDelegateProvider { _, property ->
            set(property.name, value)
            BindProperty()
        }

    private inner class BindProperty<T : Any> : ReadWriteProperty<Any?, T> {
        override fun getValue(thisRef: Any?, property: KProperty<*>): T {
            return get(property.name)
        }

        override fun setValue(thisRef: Any?, property: KProperty<*>, value: T) {
            set(property.name, value)
        }
    }

    fun <T : Any> bindCaching(value: T): PropertyDelegateProvider<Any?, ReadWriteProperty<Any?, T>> =
        PropertyDelegateProvider { _, property ->
            set(property.name, value)
            BindCachingProperty(value)
        }

    private inner class BindCachingProperty<T : Any>(private var value: T) : ReadWriteProperty<Any?, T> {
        override fun getValue(thisRef: Any?, property: KProperty<*>): T {
            return this.value
        }

        override fun setValue(thisRef: Any?, property: KProperty<*>, value: T) {
            set(property.name, value)
            this.value = value
        }
    }
}

fun newContext(): Context = Context()

fun <T> Context.autoneg(name: String, isPositive: Boolean): T =
    get(
        if (isPositive) name
        else "neg" + name.capitalize()
    )
