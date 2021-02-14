package ru.ifmo.fbsat.core.utils

import kotlinx.serialization.KSerializer
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.Json
import kotlinx.serialization.modules.SerializersModule
import ru.ifmo.fbsat.core.automaton.algorithmModule
import ru.ifmo.fbsat.core.automaton.guardModule
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import java.io.File

@Suppress("PublicApiImplicitType")
val fbsatSerializersModule by lazy {
    SerializersModule {
        include(algorithmModule)
        include(guardModule)
        include(surrogateModule)
    }
}

internal val fbsatJson by lazy {
    Json {
        serializersModule = fbsatSerializersModule
        prettyPrint = true
    }
}

object FileAsStringSerializer : KSerializer<File> {
    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("File", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: File) {
        encoder.encodeString(value.path)
    }

    override fun deserialize(decoder: Decoder): File {
        return File(decoder.decodeString())
    }
}

object InputEventSerializer : KSerializer<InputEvent> {
    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("InputEvent", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: InputEvent) {
        encoder.encodeString(value.name)
    }

    override fun deserialize(decoder: Decoder): InputEvent {
        return InputEvent(decoder.decodeString())
    }
}

object OutputEventSerializer : KSerializer<OutputEvent> {
    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("OutputEvent", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: OutputEvent) {
        encoder.encodeString(value.name)
    }

    override fun deserialize(decoder: Decoder): OutputEvent {
        return OutputEvent(decoder.decodeString())
    }
}

object InputValuesSerializer : KSerializer<InputValues> {
    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("InputValues", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: InputValues) {
        encoder.encodeString(value.values.toBinaryString())
    }

    override fun deserialize(decoder: Decoder): InputValues {
        return InputValues(decoder.decodeString())
    }
}

object OutputValuesSerializer : KSerializer<OutputValues> {
    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("OutputValues", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: OutputValues) {
        encoder.encodeString(value.values.toBinaryString())
    }

    override fun deserialize(decoder: Decoder): OutputValues {
        return OutputValues(decoder.decodeString())
    }
}
