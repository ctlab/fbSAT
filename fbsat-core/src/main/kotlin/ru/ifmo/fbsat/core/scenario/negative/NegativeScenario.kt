package ru.ifmo.fbsat.core.scenario.negative

import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.Scenario
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.outputEvent
import ru.ifmo.fbsat.core.scenario.outputValues
import ru.ifmo.fbsat.core.utils.MyLogger
import java.io.File

private val logger = MyLogger {}

data class NegativeScenario(
    override val elements: List<ScenarioElement>,
    /** One-based index of loop-back state */
    val loopPosition: Int?,
) : Scenario {
    init {
        if (loopPosition != null) {
            val loop = elements[loopPosition - 1]
            val last = elements.last()
            require(loop.outputEvent == last.outputEvent) {
                "Mismatch of output event (loopBack: ${loop.outputEvent}, last: ${last.outputEvent})"
            }
            require(loop.outputValues == last.outputValues) {
                "Mismatch of output values (loopBack: ${loop.outputValues}, last: ${last.outputValues})"
            }
        }
        require(loopPosition != null) {
            "Loopless counter-examples are not supported yet"
        }

        // println("[*] NegativeScenario with loop at $loopPosition:")
        // elements.forEachIndexed { index, elem ->
        //     println("[${index + 1}/${elements.size}] $elem")
        // }
    }

    fun slice(indicesInput: Iterable<Int>, indicesOutput: Iterable<Int>): NegativeScenario {
        return NegativeScenario(elements.map { it.slice(indicesInput, indicesOutput) }, loopPosition)
    }

    override fun toString(): String {
        return "NegativeScenarios(loopPosition=$loopPosition, elements=$elements)"
    }

    companion object {
        fun from(
            counterexample: Counterexample,
            inputEvents: List<InputEvent>,
            outputEvents: List<OutputEvent>,
            inputNames: List<String>,
            outputNames: List<String>,
        ): NegativeScenario {
            require(inputEvents.isNotEmpty())
            require(outputEvents.isNotEmpty())
            require(inputNames.isNotEmpty())
            require(outputNames.isNotEmpty())

            val elements = counterexample.states.zipWithNext { first, second ->
                ScenarioElement(
                    InputAction(
                        InputEvent.of(first.getFirstTrue(inputEvents.map { it.name })),
                        InputValues(first.getBooleanValues(inputNames))
                    ),
                    OutputAction(
                        OutputEvent.of(second.getFirstTrue(outputEvents.map { it.name })),
                        OutputValues(second.getBooleanValues(outputNames))
                    )
                ).apply {
                    ceState = second.data["_state"]
                }
            }

            val loopPosition = counterexample.loopPosition!!
            return if (loopPosition == 1) {
                logger.info("loopPosition = 1, duplicating ${elements.size} elements...")
                // Duplicate elements
                NegativeScenario(elements + elements, elements.size)
            } else {
                // Note: subtract 1. Just because.
                NegativeScenario(elements, loopPosition - 1)
            }
        }

        fun from(
            file: File,
            inputEvents: List<InputEvent>,
            outputEvents: List<OutputEvent>,
            inputNames: List<String>,
            outputNames: List<String>,
        ): List<NegativeScenario> =
            Counterexample.from(file).map { cex ->
                from(cex, inputEvents, outputEvents, inputNames, outputNames)
            }
    }
}
