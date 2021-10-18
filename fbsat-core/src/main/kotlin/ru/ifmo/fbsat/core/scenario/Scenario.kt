package ru.ifmo.fbsat.core.scenario

import kotlinx.serialization.Serializable
import kotlinx.serialization.Transient

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

    fun slice(indicesInput: Iterable<Int>, indicesOutput: Iterable<Int>): ScenarioElement {
        return ScenarioElement(
            inputAction.slice(indicesInput),
            outputAction.slice(indicesOutput)
        )
    }

    override fun toString(): String {
        return "Element($inputAction / $outputAction)"
    }

    companion object
}

val List<ScenarioElement>.preprocessed: List<ScenarioElement>
    get() = if (isEmpty()) emptyList() else
        sequence {
            yield(this@preprocessed.first())
            for ((prev, cur) in this@preprocessed.asSequence().zipWithNext())
                if (cur.outputAction.event != null || cur != prev)
                    yield(cur)
        }.toList()
