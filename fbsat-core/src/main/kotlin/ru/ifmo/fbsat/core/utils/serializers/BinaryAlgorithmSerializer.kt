package ru.ifmo.fbsat.core.utils

import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.utils.serializers.SerializerViaSurrogate
import ru.ifmo.fbsat.core.utils.serializers.Surrogate

// Look ma, no body!
object BinaryAlgorithmSerializer :
    KSerializer<BinaryAlgorithm> by SerializerViaSurrogate(BinaryAlgorithmSurrogate)

@Serializable
@SerialName("BinaryAlgorithm")
private class BinaryAlgorithmSurrogate private constructor(
    val algorithm0: String,
    val algorithm1: String,
) : Surrogate<BinaryAlgorithm> {
    fun toBinaryAlgorithm(): BinaryAlgorithm = BinaryAlgorithm(algorithm0, algorithm1)

    override fun toOriginal(): BinaryAlgorithm = toBinaryAlgorithm()

    companion object Factory : Surrogate.Factory<BinaryAlgorithm, BinaryAlgorithmSurrogate> {
        override val surrogateSerializer: KSerializer<BinaryAlgorithmSurrogate> = serializer()

        override fun from(original: BinaryAlgorithm): BinaryAlgorithmSurrogate {
            val a0 = original.algorithm0.toBinaryString()
            val a1 = original.algorithm1.toBinaryString()
            return BinaryAlgorithmSurrogate(a0, a1)
        }
    }
}
