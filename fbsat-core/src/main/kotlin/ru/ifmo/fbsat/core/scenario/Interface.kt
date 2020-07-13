package ru.ifmo.fbsat.core.scenario

import ru.ifmo.fbsat.core.utils.toBinaryString

// Event

sealed class Event : GenericEvent {
    abstract val name: String
}

data class InputEvent(override val name: String) : Event(),
    GenericInputEvent {
    companion object {
        @JvmStatic
        fun of(name: String?): InputEvent? = name?.let { InputEvent(it) }
    }
}

data class OutputEvent(override val name: String) : Event(),
    GenericOutputEvent {
    companion object {
        @JvmStatic
        fun of(name: String?): OutputEvent? = name?.let { OutputEvent(it) }
    }
}

// Values

sealed class Values : GenericValues {
    abstract val values: List<Boolean>

    operator fun get(index: Int): Boolean = values[index]
}

data class InputValues(override val values: List<Boolean>) : Values(),
    GenericInputValues {
    companion object {
        fun empty(): InputValues = InputValues(emptyList())
        fun zeros(size: Int): InputValues = InputValues(List(size) { false })
    }
}

data class OutputValues(override val values: List<Boolean>) : Values(),
    GenericOutputValues {
    companion object {
        fun empty(): OutputValues = OutputValues(emptyList())
        fun zeros(size: Int): OutputValues = OutputValues(List(size) { false })
    }
}

// Action

sealed class ScenarioAction<E, V> : GenericScenarioAction<E, V>
    where E : Event,
          V : Values {
    abstract val event: E?
    abstract val values: V
    override fun toString(): String = "${event?.name ?: 'ε'}[${values.values.toBinaryString()}]"
}

data class InputAction(
    override val event: InputEvent?,
    override val values: InputValues
) : ScenarioAction<InputEvent, InputValues>(),
    GenericScenarioInputAction<InputEvent, InputValues>

data class OutputAction(
    override val event: OutputEvent?,
    override val values: OutputValues
) : ScenarioAction<OutputEvent, OutputValues>(),
    GenericScenarioOutputAction<OutputEvent, OutputValues>
