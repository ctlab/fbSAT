package ru.ifmo.fbsat.core.scenario

import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.automaton.OutputValues
import ru.ifmo.fbsat.core.utils.Globals

interface GenericScenario<out T : GenericScenario.Element> {
    val elements: List<T>

    interface Element
}

interface Scenario : GenericScenario<ScenarioElement>

val Scenario.inputActions: List<InputAction>
    get() = elements.map { it.inputAction }
val Scenario.inputActionsSeq: Sequence<InputAction>
    get() = elements.asSequence().map { it.inputAction }

data class ScenarioElement(
    val inputAction: InputAction,
    val outputAction: OutputAction
) : GenericScenario.Element {
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

val auxScenarioElement: ScenarioElement by lazy {
    ScenarioElement(
        InputAction(
            event = null,
            values = InputValues.empty()
        ),
        OutputAction(
            event = null,
            values = Globals.INITIAL_OUTPUT_VALUES
        )
    )
}
