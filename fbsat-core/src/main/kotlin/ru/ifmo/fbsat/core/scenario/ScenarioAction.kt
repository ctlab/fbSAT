package ru.ifmo.fbsat.core.scenario

import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.automaton.OutputValues
import ru.ifmo.fbsat.core.utils.toBinaryString

sealed class ScenarioAction

data class InputAction(
    val event: InputEvent?,
    val values: InputValues
) : ScenarioAction() {
    override fun toString(): String {
        return "${event?.name ?: 'ε'}[${values.values.toBinaryString()}]"
    }
}

data class OutputAction(
    val event: OutputEvent?,
    val values: OutputValues
) : ScenarioAction() {
    override fun toString(): String {
        return "${event?.name ?: 'ε'}[${values.values.toBinaryString()}]"
    }
}
