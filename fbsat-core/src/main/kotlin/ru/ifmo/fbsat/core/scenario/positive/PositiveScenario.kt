package ru.ifmo.fbsat.core.scenario.positive

import kotlinx.serialization.Serializable
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.json.Json
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.Scenario
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.negative.Counterexample
import ru.ifmo.fbsat.core.scenario.preprocessed
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.sourceAutoGzip
import ru.ifmo.fbsat.core.utils.toBooleanList
import ru.ifmo.fbsat.core.utils.useLines
import java.io.File

private val logger = MyLogger {}

@Serializable
data class PositiveScenario(
    override val elements: List<ScenarioElement>,
) : Scenario {
    fun slice(indicesInput: Iterable<Int>, indicesOutput: Iterable<Int>): PositiveScenario {
        return PositiveScenario(elements.map { it.slice(indicesInput, indicesOutput) })
    }

    companion object {
        private val regexAction by lazy {
            // Regex("""^(?<type>in|out)=(?<event>.*?)\[(?<values>[01]*)]$""")
            Regex("""^(in|out)=(.*?)\[([01]*)]$""")
        }

        private val regexOutputValues by lazy {
            Regex("""out=.*?\[([01]*)]""")
        }

        // fromWeirdFormat(String)
        fun fromString(
            s: String,
            initialOutputValues: OutputValues,
            preprocess: Boolean = true,
        ): PositiveScenario {
            var lastOutputValues = initialOutputValues
            val elements: List<ScenarioElement> = s
                .splitToSequence(";")
                .map { it.trim() }
                .filter { it.isNotEmpty() }
                .map { regexAction.matchEntire(it) }
                .requireNoNulls()
                .map {
                    val (type, event, values) = it.destructured
                    when (type) {
                        "in" -> InputAction(
                            event = InputEvent(event),
                            values = InputValues(values.toBooleanList())
                        )
                        "out" -> OutputAction(
                            event = OutputEvent(event),
                            values = OutputValues(values.toBooleanList())
                        )
                        else -> error("Unsupported action type '$type'")
                    }
                }
                .zipWithNext()
                .mapNotNull { (first, second) ->
                    if (first is InputAction)
                        first to second
                    else
                        null
                }
                .map { (first, second) ->
                    when (second) {
                        is InputAction -> {
                            ScenarioElement(first, OutputAction(null, lastOutputValues))
                        }
                        is OutputAction -> {
                            lastOutputValues = second.values
                            ScenarioElement(first, second)
                        }
                    }
                }
                .toList()

            return if (preprocess)
                PositiveScenario(elements.preprocessed)
            else
                PositiveScenario(elements)
        }

        // fromWeirdFormat(File)
        fun fromFile(
            file: File,
            initialOutputValues: OutputValues,
            preprocess: Boolean = true,
        ): List<PositiveScenario> {
            return file.sourceAutoGzip().useLines { lines ->
                val scenarios: MutableList<PositiveScenario> = mutableListOf()
                var numberOfScenarios = 0

                for ((index, line) in lines.withIndex()) {
                    if (index == 0) {
                        numberOfScenarios = line.toInt()
                    } else {
                        scenarios.add(fromString(line, initialOutputValues, preprocess))
                    }
                }

                if (scenarios.size != numberOfScenarios)
                    logger.warn("Number of scenarios mismatch: specified $numberOfScenarios, but found ${scenarios.size}")

                scenarios
            }
        }

        fun fromJson(s: String, format: Json = Json): List<PositiveScenario> {
            return format.decodeFromString(s)
        }

        fun fromJson(file: File, format: Json = Json): List<PositiveScenario> {
            val s = file.readText()
            return fromJson(s, format)
        }

        fun from(
            ce: Counterexample,
            inputEvents: List<InputEvent>,
            outputEvents: List<OutputEvent>,
            inputNames: List<String>,
            outputNames: List<String>,
            preprocess: Boolean = true,
        ): PositiveScenario {
            val elements = ce.states.zipWithNext { first, second ->
                ScenarioElement(
                    InputAction(
                        event = InputEvent.of(first.getFirstTrue(inputEvents.map { it.name })),
                        values = InputValues(first.getBooleanValues(inputNames))
                    ),
                    OutputAction(
                        event = OutputEvent.of(second.getFirstTrue(outputEvents.map { it.name })),
                        values = OutputValues(second.getBooleanValues(outputNames))
                    )
                )
            }
            return if (preprocess)
                PositiveScenario(elements.preprocessed)
            else
                PositiveScenario(elements)
        }

        fun fromAutoReqCnf(
            ce: Counterexample,
            inputNames: List<String>,
            outputNames: List<String>,
            preprocess: Boolean = true,
        ): PositiveScenario {
            val elements = ce.states.zipWithNext { first, second ->
                ScenarioElement(
                    InputAction(
                        event = InputEvent("REQ"),
                        values = InputValues(first.getBooleanValues(inputNames))
                    ),
                    OutputAction(
                        event = OutputEvent("CNF"),
                        values = OutputValues(second.getBooleanValues(outputNames))
                    )
                )
            }
            return if (preprocess)
                PositiveScenario(elements.preprocessed)
            else
                PositiveScenario(elements)
        }

        fun fromSmvout(
            file: File,
            inputEvents: List<InputEvent>,
            outputEvents: List<OutputEvent>,
            inputNames: List<String>,
            outputNames: List<String>,
            preprocess: Boolean = true,
        ): List<PositiveScenario> =
            Counterexample.fromSmvout(file).map { ce ->
                from(ce, inputEvents, outputEvents, inputNames, outputNames, preprocess)
            }
    }
}
