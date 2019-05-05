package ru.ifmo.fbsat.core.utils

interface Observer {
    fun update()
}

interface Observable<T : Observer> {
    fun subscribe(subscriber: T)
    fun updateAll()
}

abstract class AbstractObservable<T : Observer> internal constructor() : Observable<T> {
    private val subscribers: MutableList<T> = mutableListOf()

    final override fun subscribe(subscriber: T) {
        subscribers.add(subscriber)
    }

    final override fun updateAll() {
        subscribers.forEach(Observer::update)
    }
}
