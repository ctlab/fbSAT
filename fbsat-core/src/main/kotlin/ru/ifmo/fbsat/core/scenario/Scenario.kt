package ru.ifmo.fbsat.core.scenario

import ru.ifmo.fbsat.core.utils.Globals

interface Scenario : GenericScenario<ScenarioElement, InputAction, OutputAction>

data class ScenarioElement(
    override val inputAction: InputAction,
    override val outputAction: OutputAction
) : GenericScenario.Element<InputAction, OutputAction> {
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

val List<ScenarioElement>.preprocessed: List<ScenarioElement>
    get() = if (isEmpty()) emptyList() else
        sequence {
            yield(this@preprocessed.first())
            for ((prev, cur) in this@preprocessed.asSequence().zipWithNext())
                if (cur.outputAction.event != null || cur != prev)
                    yield(cur)
        }.toList()
