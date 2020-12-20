package ru.ifmo.fbsat.core.scenario

import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.toBooleanList

// Event

sealed class Event : GenericEvent {
    abstract val name: String

    final override fun toString(): String = name
}

data class InputEvent(override val name: String) : Event(), GenericInputEvent {
    // override fun toString(): String = super.toString()

    companion object {
        @JvmStatic
        fun of(name: String?): InputEvent? = name?.let { InputEvent(it) }
    }
}

data class OutputEvent(override val name: String) : Event(), GenericOutputEvent {
    // override fun toString(): String = super.toString()

    companion object {
        @JvmStatic
        fun of(name: String?): OutputEvent? = name?.let { OutputEvent(it) }
    }
}

// Values

sealed class Values : GenericValues {
    abstract val values: List<Boolean>

    operator fun get(index: Int): Boolean = values[index]

    final override fun toString(): String = values.toBinaryString()
}

data class InputValues(override val values: List<Boolean>) : Values(), GenericInputValues {
    constructor(values: BooleanArray) : this(values.asList())
    constructor(values: String) : this(values.toBooleanList())

    companion object {
        fun empty(): InputValues = InputValues(emptyList())
        fun zeros(size: Int): InputValues = InputValues(List(size) { false })
    }
}

data class OutputValues(override val values: List<Boolean>) : Values(), GenericOutputValues {
    constructor(values: BooleanArray) : this(values.asList())
    constructor(values: String) : this(values.toBooleanList())

    companion object {
        fun empty(): OutputValues = OutputValues(emptyList())
        fun zeros(size: Int): OutputValues = OutputValues(List(size) { false })
    }
}

// Action

sealed class ScenarioAction<E, V> : GenericScenarioAction<E, V>
    where E : Event,
          V : Values {
    final override fun toString(): String = "${event ?: 'ε'}[$values]"
}

data class InputAction(
    override val event: InputEvent?,
    override val values: InputValues,
) : ScenarioAction<InputEvent, InputValues>(),
    GenericScenarioInputAction<InputEvent, InputValues> {
    // override fun toString(): String = super.toString()
}

data class OutputAction(
    override val event: OutputEvent?,
    override val values: OutputValues,
) : ScenarioAction<OutputEvent, OutputValues>(),
    GenericScenarioOutputAction<OutputEvent, OutputValues> {
    // override fun toString(): String = super.toString()
}
