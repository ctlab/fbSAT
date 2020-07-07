package ru.ifmo.fbsat.core.scenario

import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.automaton.OutputValues

interface Scenario {
    val elements: List<ScenarioElement>
}

data class ScenarioElement(
    val inputAction: InputAction,
    val outputAction: OutputAction
) {
    var nodeId: Int? = null
    var ceState: String? = null // FIXME: remove

    val inputEvent: InputEvent? = inputAction.event
    val inputValues: InputValues = inputAction.values
    val outputEvent: OutputEvent? = outputAction.event
    val outputValues: OutputValues = outputAction.values

    override fun toString(): String {
        return "Element($inputAction / $outputAction)"
    }
}

val List<ScenarioElement>.preprocessed: List<ScenarioElement>
    get() = if (isEmpty()) emptyList() else
        sequence {
            yield(this@preprocessed.first())
            for ((prev, cur) in this@preprocessed.asSequence().zipWithNext())
                if (cur.outputAction.event != null || cur != prev)
                    yield(cur)
        }.toList()
