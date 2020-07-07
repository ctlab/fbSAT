package ru.ifmo.fbsat.core.scenario

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.automaton.OutputValues
import java.io.File

interface MultiScenario {
    val elements: List<MultiScenarioElement>
}

val MultiScenario.modularInputActions: List<MultiArray<InputAction>>
    get() = elements.map { it.modularInputAction }
val MultiScenario.modularInputActionsSeq: Sequence<MultiArray<InputAction>>
    get() = elements.asSequence().map { it.modularInputAction }

typealias MultiScenarioElement = MultiArray<ScenarioElement>

val MultiScenarioElement.modularInputAction: MultiArray<InputAction>
    get() = map { it.inputAction }
val MultiScenarioElement.modularOutputAction: MultiArray<OutputAction>
    get() = map { it.outputAction }
val MultiScenarioElement.modularInputEvent: MultiArray<InputEvent?>
    get() = modularInputAction.map { it.event }
val MultiScenarioElement.modularInputValues: MultiArray<InputValues>
    get() = modularInputAction.map { it.values }
val MultiScenarioElement.modularOutputEvent: MultiArray<OutputEvent?>
    get() = modularOutputAction.map { it.event }
val MultiScenarioElement.modularOutputValues: MultiArray<OutputValues>
    get() = modularOutputAction.map { it.values }

// data class MultiScenarioElement(
//     val modularElement: MultiArray<ScenarioElement>
// ) {
//     val modularInputAction: MultiArray<InputAction> = modularElement.map { it.inputAction }
//     val modularOutputAction: MultiArray<OutputAction> = modularElement.map { it.outputAction }
//     val modularInputEvent: MultiArray<InputEvent?> = modularInputAction.map { it.event }
//     val modularInputValues: MultiArray<InputValues> = modularInputAction.map { it.values }
//     val modularOutputEvent: MultiArray<OutputEvent?> = modularOutputAction.map { it.event }
//     val modularOutputValues: MultiArray<OutputValues> = modularOutputAction.map { it.values }
//
//     override fun toString(): String {
//         return "MultiElement(${modularElement.values})"
//     }
// }

class PositiveMultiScenario(
    override val elements: List<MultiScenarioElement>
) : MultiScenario {
    companion object {
        fun fromFile(file: File): List<PositiveMultiScenario> {
            TODO()
        }
    }
}
