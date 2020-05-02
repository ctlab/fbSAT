package ru.ifmo.fbsat.core.scenario2

import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.automaton.OutputValues
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.toBinaryString

sealed class ScenarioAction2

data class InputAction2(
    val event: InputEvent,
    val values: InputValues
) : ScenarioAction2() {
    override fun toString(): String {
        return "${event.name}[${values.values.toBinaryString()}]"
    }
}

data class OutputAction2(
    val event: OutputEvent,
    val values: OutputValues
) : ScenarioAction2() {
    override fun toString(): String {
        return "${event.name}[${values.values.toBinaryString()}]"
    }
}

data class ScenarioElement2(
    val inputAction: InputAction2,
    val outputActions: List<OutputAction2>
) {
    var nodeId: Int? = null
    var ceState: String? = null // FIXME: remove

    val inputEvent: InputEvent = inputAction.event
    val inputValues: InputValues = inputAction.values

    override fun toString(): String {
        return "Element($inputAction / $outputActions)"
    }
}

val List<ScenarioElement2>.preprocessed: List<ScenarioElement2>
    get() = this.also {
        log.warn("Scenario preprocessing is not implemented yet!")
    }
