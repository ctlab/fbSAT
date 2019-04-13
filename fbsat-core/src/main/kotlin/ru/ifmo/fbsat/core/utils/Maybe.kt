package ru.ifmo.fbsat.core.utils

sealed class Maybe<out T> {
    abstract val value: T
}

data class Some<out T>(override val value: T) : Maybe<T>() {
    override fun toString(): String = "Some($value)"
}

object None : Maybe<Nothing>() {
    override val value: Nothing
        get() = throw RuntimeException("Can't get from None")

    override fun toString(): String = "None"
}
