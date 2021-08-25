@file:Suppress("unused")

package ru.ifmo.fbsat.core.utils.serializers

import kotlinx.serialization.KSerializer
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encodeToString
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import ru.ifmo.fbsat.core.automaton.guard.BooleanExpressionGuard
import ru.ifmo.fbsat.core.utils.boolexpr.randomBooleanExpression
import kotlin.random.Random

class SerializerViaSurrogate<T, out ST : Surrogate<T>>(
    private val surrogateFactory: Surrogate.Factory<T, ST>,
) : KSerializer<T> {
    private val surrogateSerializer: KSerializer<ST> = surrogateFactory.surrogateSerializer

    override val descriptor: SerialDescriptor = surrogateSerializer.descriptor

    override fun serialize(encoder: Encoder, value: T) {
        val surrogate = surrogateFactory.from(value)
        encoder.encodeSerializableValue(surrogateSerializer, surrogate)
    }

    override fun deserialize(decoder: Decoder): T {
        val surrogate: Surrogate<T> = decoder.decodeSerializableValue(surrogateSerializer)
        return surrogate.toOriginal()
    }
}

fun main() {
    val X = 3
    val inputNames = (1..X).map { "x$it" }

    val guard = BooleanExpressionGuard(randomBooleanExpression(5, inputNames, Random(42)))
    println("guard = ${guard.toSimpleString()}")

    val guardJson = fbsatJson.encodeToString(guard)
    println("guard -> json = $guardJson")

    val guard2: BooleanExpressionGuard = fbsatJson.decodeFromString(guardJson)
    println("json -> guard = ${guard2.toSimpleString()}")
}
