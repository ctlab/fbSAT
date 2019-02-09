package ru.ifmo.fbsat.scenario

import java.io.File

class NegativeScenario(
    val elements: List<ScenarioElement>,
    val loopPosition: Int?  // Note: 1-based
) {
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

        println("[*] NegativeScenario with loop at $loopPosition:")
        elements.forEachIndexed { index, elem ->
            println("[${index + 1}/${elements.size}] $elem")
        }
    }

    override fun toString(): String {
        return "NegativeScenarios(loopPosition=$loopPosition, elements=$elements)"
    }

    companion object {
        fun fromFile(
            file: File,
            inputEvents: List<String>,
            outputEvents: List<String>,
            inputNames: List<String>,
            outputNames: List<String>
        ): List<NegativeScenario> =
            CounterExample.fromFile(file).map {
                it.toNegativeScenario(inputEvents, outputEvents, inputNames, outputNames)
            }
    }
}

fun CounterExample.toNegativeScenario(
    inputEvents: List<String>,
    outputEvents: List<String>,
    inputNames: List<String>,
    outputNames: List<String>
): NegativeScenario {
    require(inputEvents.isNotEmpty())
    require(outputEvents.isNotEmpty())
    require(inputNames.isNotEmpty())
    require(outputNames.isNotEmpty())

    val elements = sequence {
        yield(
            ScenarioElement(
                "",
                "",
                states.first().getFirstTrue(outputEvents),
                states.first().getBooleanString(outputNames)
            )
        )
        yieldAll(
            // states.dropLast(1).zipWithNext { first, second ->
            states.zipWithNext { first, second ->
                ScenarioElement(
                    first.getFirstTrue(inputEvents)!!,
                    first.getBooleanString(inputNames),
                    second.getFirstTrue(outputEvents),
                    second.getBooleanString(outputNames)
                )
            }
        )
    }.toList()

    return NegativeScenario(elements, loopPosition)
}
