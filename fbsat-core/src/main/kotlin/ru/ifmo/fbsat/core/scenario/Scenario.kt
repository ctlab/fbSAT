package ru.ifmo.fbsat.core.scenario

import kotlinx.serialization.Serializable
import kotlinx.serialization.Transient
import ru.ifmo.fbsat.core.utils.Globals

interface Scenario : GenericScenario<ScenarioElement>

@Serializable
data class ScenarioElement(
    override val inputAction: InputAction,
    override val outputAction: OutputAction,
) : GenericScenario.Element<InputAction, OutputAction> {
    @Transient
    override var nodeId: Int? = null

    @Transient
    var ceState: String? = null // FIXME: remove

    @Transient
    val inputEvent: InputEvent? = inputAction.event

    @Transient
    val inputValues: InputValues = inputAction.values

    @Transient
    val outputEvent: OutputEvent? = outputAction.event

    @Transient
    val outputValues: OutputValues = outputAction.values

    override fun toString(): String {
        return "Element($inputAction / $outputAction)"
    }

    companion object
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
