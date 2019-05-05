package ru.ifmo.fbsat.core.utils

sealed class Option<out T> {
    abstract fun isEmpty(): Boolean

    inline fun <U> map(f: (T) -> U): Option<U> =
        when (this) {
            is None -> None
            is Some<T> -> Some(f(value))
        }

    companion object {
        fun <T> empty(): Option<T> = None
        fun <T> just(value: T): Option<T> = Some(value)
        operator fun <T> invoke(value: T): Option<T> = just(value)
        fun <T> fromNullable(value: T?): Option<T> = if (value != null) just(value) else empty()
    }
}

object None : Option<Nothing>() {
    override fun isEmpty(): Boolean = true
    override fun toString(): String = "None"
}

data class Some<out T>(val value: T) : Option<T>() {
    override fun isEmpty(): Boolean = false
    override fun toString(): String = "Some($value)"
}
