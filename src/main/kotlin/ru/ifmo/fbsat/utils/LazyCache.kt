package ru.ifmo.fbsat.utils

import kotlin.properties.ReadOnlyProperty
import kotlin.reflect.KProperty

class LazyCache {
    private val properties: MutableList<LazyCachedValue<*>> = mutableListOf()

    fun subscribe(listener: LazyCachedValue<*>) {
        properties.add(listener)
    }

    fun <T> subscribe(init: () -> T): LazyCachedValue<T> = LazyCachedValue(init).also { subscribe(it) }

    operator fun <T> invoke(init: () -> T): LazyCachedValue<T> = subscribe(init)

    fun invalidate() {
        // println("Invalidating LazyCache...")
        properties.forEach(LazyCachedValue<*>::invalidate)
    }
}

class LazyCachedValue<out T>(
    private val init: () -> T
) : ReadOnlyProperty<Any, T> {
    private var cached: Maybe<T> = None

    fun invalidate() {
        // println("Invalidating LazyCachedValue...")
        cached = None
    }

    override fun getValue(thisRef: Any, property: KProperty<*>): T {
        if (cached == None) {
            // println("Initializing LazyCachedValue(${property.name})...")
            cached = Some(init())
        }
        return cached.value
    }
}
