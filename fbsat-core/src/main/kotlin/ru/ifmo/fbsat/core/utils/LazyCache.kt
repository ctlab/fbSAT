package ru.ifmo.fbsat.core.utils

import kotlin.properties.ReadOnlyProperty
import kotlin.reflect.KProperty

class LazyCache() : AbstractObservable<LazyCachedValue<*>>(), Observer {
    constructor(vararg observables: Observable<Observer>) : this() {
        observables.forEach { it.subscribe(this) }
    }

    fun <T> subscribe(init: () -> T): LazyCachedValue<T> =
        LazyCachedValue(init).also { subscribe(it) }

    operator fun <T> invoke(init: () -> T): LazyCachedValue<T> = subscribe(init)

    fun invalidate() {
        // println("Invalidating LazyCache...")
        updateAll()
    }

    override fun update() {
        invalidate()
    }
}

class LazyCachedValue<out T>(
    private val init: () -> T
) : ReadOnlyProperty<Any, T>, Observer {
    private var cached: Option<T> = None

    fun invalidate() {
        // println("Invalidating LazyCachedValue...")
        cached = None
    }

    override fun update() {
        invalidate()
    }

    override fun getValue(thisRef: Any, property: KProperty<*>): T =
        when (val c = cached) {
            is None -> init().also { cached = Some(it) }
            // .also { println("Calculated ${property.name} = $it...") }
            is Some<T> -> c.value
            // .also { println("Returning already stored ${property.name} = ${it}...") }
        }
}

private class LazyCacheExampleObject {
    private val _data = ObservableMutableList<Int>()
    private val lazyCache = LazyCache(_data)

    val data: List<Int> by lazyCache {
        println("==> Sorting data...")
        _data.sorted()
    }

    fun add(value: Int): Boolean {
        return _data.add(value)
    }
}

fun main() {
    println("==> Creating object...")
    val a = LazyCacheExampleObject()
    println("  -> Object created")

    println("a.data = ${a.data}")
    println("a.data = ${a.data}")

    println("==> Adding 42...")
    a.add(42)
    println("  -> Added 42")
    println("a.data = ${a.data}")
    println("a.data = ${a.data}")

    println("==> Adding more values...")
    repeat(10) {
        a.add((1..100).random().also { println("  -> adding $it") })
    }
    println(" -> Added some values")
    println("a.data = ${a.data}")
    println("a.data = ${a.data}")
}
