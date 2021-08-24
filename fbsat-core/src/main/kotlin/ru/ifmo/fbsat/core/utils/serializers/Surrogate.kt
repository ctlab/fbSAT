package ru.ifmo.fbsat.core.utils.serializers

import kotlinx.serialization.KSerializer

interface Surrogate<out T> {
    fun toOriginal(): T

    interface Factory<T, ST : Surrogate<T>> {
        val surrogateSerializer: KSerializer<ST> // Note: just use `= serializer()` as implementation

        fun from(original: T): ST
    }
}
