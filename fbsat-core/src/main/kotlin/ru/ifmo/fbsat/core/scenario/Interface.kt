package ru.ifmo.fbsat.core.scenario

import kotlinx.serialization.Serializable
import ru.ifmo.fbsat.core.utils.serializers.InputEventSerializer
import ru.ifmo.fbsat.core.utils.serializers.InputValuesSerializer
import ru.ifmo.fbsat.core.utils.serializers.OutputEventSerializer
import ru.ifmo.fbsat.core.utils.serializers.OutputValuesSerializer
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.toBooleanList

// Event

@Serializable
sealed class Event : GenericEvent {
    abstract val name: String

    final override fun toString(): String = name
}

@Serializable(with = InputEventSerializer::class)
data class InputEvent(override val name: String) : Event(), GenericInputEvent {
    // override fun toString(): String = super.toString()

    companion object {
        @JvmStatic
        fun of(name: String?): InputEvent? = name?.let { InputEvent(it) }
    }
}

@Serializable(with = OutputEventSerializer::class)
data class OutputEvent(override val name: String) : Event(), GenericOutputEvent {
    // override fun toString(): String = super.toString()

    companion object {
        @JvmStatic
        fun of(name: String?): OutputEvent? = name?.let { OutputEvent(it) }
    }
}

// Values

@Serializable
sealed class Values : GenericValues {
    abstract val values: List<Boolean>

    operator fun get(index: Int): Boolean = values[index]

    final override fun toString(): String = values.toBinaryString()
}

@Serializable(with = InputValuesSerializer::class)
data class InputValues(override val values: List<Boolean>) : Values(), GenericInputValues {
    constructor(values: BooleanArray) : this(values.asList())
    constructor(values: String) : this(values.toBooleanList())

    companion object {
        fun empty(): InputValues = InputValues(emptyList())
        fun zeros(size: Int): InputValues = InputValues(List(size) { false })
    }
}

@Serializable(with = OutputValuesSerializer::class)
data class OutputValues(override val values: List<Boolean>) : Values(), GenericOutputValues {
    constructor(values: BooleanArray) : this(values.asList())
    constructor(values: String) : this(values.toBooleanList())

    companion object {
        fun empty(): OutputValues = OutputValues(emptyList())
        fun zeros(size: Int): OutputValues = OutputValues(List(size) { false })
    }
}

// Action

@Serializable
sealed class ScenarioAction<E, V> : GenericScenarioAction<E, V>
    where E : Event,
          V : Values {
    final override fun toString(): String = "${event ?: 'ε'}[$values]"
}

@Serializable
data class InputAction(
    override val event: InputEvent?,
    override val values: InputValues,
) : ScenarioAction<InputEvent, InputValues>(),
    GenericScenarioInputAction<InputEvent, InputValues> {
    // override fun toString(): String = super.toString()
}

@Serializable
data class OutputAction(
    override val event: OutputEvent?,
    override val values: OutputValues,
) : ScenarioAction<OutputEvent, OutputValues>(),
    GenericScenarioOutputAction<OutputEvent, OutputValues> {
    // override fun toString(): String = super.toString()
}
