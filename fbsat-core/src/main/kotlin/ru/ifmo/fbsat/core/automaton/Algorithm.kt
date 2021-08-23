package ru.ifmo.fbsat.core.automaton

import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.modules.SerializersModule
import kotlinx.serialization.modules.polymorphic
import kotlinx.serialization.modules.subclass
import kotlinx.serialization.serializer
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.toBooleanArray

@Suppress("PublicApiImplicitType")
val algorithmModule = SerializersModule {
    polymorphic(Algorithm::class) {
        subclass(BinaryAlgorithm::class)
    }
}

interface Algorithm {
    fun eval(outputValues: OutputValues): OutputValues
    fun toSimpleString(): String
}

object BinaryAlgorithmSerializer : KSerializer<BinaryAlgorithm> {
    override val descriptor: SerialDescriptor = serializer<Pair<String, String>>().descriptor

    override fun serialize(encoder: Encoder, value: BinaryAlgorithm) {
        val a0 = value.algorithm0.toBinaryString()
        val a1 = value.algorithm1.toBinaryString()
        encoder.encodeSerializableValue(serializer(), Pair(a0, a1))
    }

    override fun deserialize(decoder: Decoder): BinaryAlgorithm {
        val (a0, a1) = decoder.decodeSerializableValue<Pair<String, String>>(serializer())
        return BinaryAlgorithm(a0, a1)
    }
}

@Serializable(with = BinaryAlgorithmSerializer::class)
@SerialName("BinaryAlgorithm")
class BinaryAlgorithm(val algorithm0: BooleanArray, val algorithm1: BooleanArray) : Algorithm {
    constructor(algorithm0: Collection<Boolean>, algorithm1: Collection<Boolean>) :
        this(algorithm0.toBooleanArray(), algorithm1.toBooleanArray())

    constructor(algorithm0: String, algorithm1: String) :
        this(algorithm0.toBooleanArray(), algorithm1.toBooleanArray())

    init {
        require(algorithm0.size == algorithm1.size) {
            "Algorithm size mismatch (algo0 = $algorithm0, algo1 = $algorithm1)"
        }
    }

    override fun eval(outputValues: OutputValues): OutputValues {
        require(outputValues.values.size == algorithm0.size) { "Number (${outputValues.values.size}) of output values (${outputValues.values.toList()}) and algorithm size (${algorithm0.size}) mismatch" }
        return OutputValues(outputValues.values.mapIndexed { i, b -> if (b) algorithm1[i] else algorithm0[i] })
    }

    override fun toSimpleString(): String {
        return "0:${algorithm0.toBinaryString()}, 1:${algorithm1.toBinaryString()}"
    }

    fun toFbtString(): String {
        return "${algorithm0.toBinaryString()}_${algorithm1.toBinaryString()}"
    }

    fun toST(outputNames: List<String>): String {
        // require(outputNames.size == algorithm0.size) { "Wrong number of output names" }

        return outputNames.indices.joinToString("") { i ->
            val name = outputNames[i]
            val a0 = algorithm0[i]
            val a1 = algorithm1[i]
            if (a0 == a1)
                "$name:=${a0.toString().uppercase()};"
            else
                "$name:=NOT $name;"
        }
    }

    override fun toString(): String {
        return "BinaryAlgorithm(${this.toSimpleString()})"
    }
}
