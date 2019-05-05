package ru.ifmo.fbsat.core.utils

class ObservableMutableList<E>(
    private val delegate: MutableList<E> = mutableListOf()
) : MutableList<E> by delegate, AbstractObservable<Observer>() {
    override fun add(element: E): Boolean {
        return delegate.add(element).also { updateAll() }
    }

    override fun add(index: Int, element: E) {
        return delegate.add(index, element).also { updateAll() }
    }

    override fun addAll(index: Int, elements: Collection<E>): Boolean {
        return delegate.addAll(index, elements).also { updateAll() }
    }

    override fun addAll(elements: Collection<E>): Boolean {
        return delegate.addAll(elements).also { updateAll() }
    }

    override fun clear() {
        return delegate.clear().also { updateAll() }
    }

    override fun remove(element: E): Boolean {
        return delegate.remove(element).also { updateAll() }
    }

    override fun removeAll(elements: Collection<E>): Boolean {
        return delegate.removeAll(elements).also { updateAll() }
    }

    override fun removeAt(index: Int): E {
        return delegate.removeAt(index).also { updateAll() }
    }

    override fun retainAll(elements: Collection<E>): Boolean {
        return delegate.retainAll(elements).also { updateAll() }
    }

    override fun set(index: Int, element: E): E {
        return delegate.set(index, element).also { updateAll() }
    }

    override fun toString(): String {
        return delegate.toString()
    }
}
