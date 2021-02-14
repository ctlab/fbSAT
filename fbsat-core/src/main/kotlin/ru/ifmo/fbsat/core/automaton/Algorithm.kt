package ru.ifmo.fbsat.core.automaton

import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.descriptors.buildClassSerialDescriptor
import kotlinx.serialization.descriptors.element
import kotlinx.serialization.encodeToString
import kotlinx.serialization.encoding.CompositeDecoder
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.encoding.decodeStructure
import kotlinx.serialization.encoding.encodeStructure
import kotlinx.serialization.json.Json
import kotlinx.serialization.modules.SerializersModule
import kotlinx.serialization.modules.polymorphic
import kotlinx.serialization.modules.subclass
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
    override val descriptor: SerialDescriptor =
        buildClassSerialDescriptor("BinaryAlgorithm") {
            element<String>("algorithm0")
            element<String>("algorithm1")
        }

    override fun serialize(encoder: Encoder, value: BinaryAlgorithm) {
        val a0 = value.algorithm0.toBinaryString()
        val a1 = value.algorithm1.toBinaryString()
        encoder.encodeStructure(descriptor) {
            encodeStringElement(descriptor, 0, a0)
            encodeStringElement(descriptor, 1, a1)
        }
    }

    @ExperimentalSerializationApi
    override fun deserialize(decoder: Decoder): BinaryAlgorithm {
        return decoder.decodeStructure(descriptor) {
            lateinit var a0: String
            lateinit var a1: String
            if (decodeSequentially()) {
                a0 = decodeStringElement(descriptor, 0)
                a1 = decodeStringElement(descriptor, 1)
            } else while (true) {
                when (val index = decodeElementIndex(descriptor)) {
                    0 -> a0 = decodeStringElement(descriptor, 0)
                    1 -> a1 = decodeStringElement(descriptor, 1)
                    CompositeDecoder.DECODE_DONE -> break
                    else -> error("Unexpected index: $index")
                }
            }
            BinaryAlgorithm(a0, a1)
        }
    }
}

@Serializable(with = BinaryAlgorithmSerializer::class)
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
                "$name:=${a0.toString().toUpperCase()};"
            else
                "$name:=NOT $name;"
        }
    }

    override fun toString(): String {
        return "BinaryAlgorithm(${this.toSimpleString()})"
    }
}

fun main() {
    val format = Json {
        serializersModule = SerializersModule {
            include(algorithmModule)
            include(guardModule)
        }
        prettyPrint = true
    }

    val a0 = "0101"
    val a1 = "1110"
    val a: Algorithm = BinaryAlgorithm(a0, a1)
    println("a = $a")
    val s = format.encodeToString(a)
    println("s = $s")
    val b: Algorithm = format.decodeFromString(s)
    println("b = $b")
}
