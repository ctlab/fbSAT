package ru.ifmo.fbsat.core.scenario.positive

import kotlinx.serialization.Serializable
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.json.Json
import okio.buffer
import okio.source
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.Scenario
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.negative.THE_Counterexample
import ru.ifmo.fbsat.core.scenario.preprocessed
import ru.ifmo.fbsat.core.utils.Globals
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
    companion object {
        fun fromJsonFile(file: File): List<PositiveScenario> {
            val s = file.source().buffer().use { it.readUtf8() }
            return Json.decodeFromString(s)
        }

        fun fromFile(file: File, preprocess: Boolean = true): List<PositiveScenario> {
            return file.sourceAutoGzip().useLines { lines ->
                val scenarios: MutableList<PositiveScenario> = mutableListOf()
                var numberOfScenarios = 0

                for ((index, line) in lines.withIndex()) {
                    if (index == 0) {
                        numberOfScenarios = line.toInt()
                    } else {
                        scenarios.add(fromString(line, preprocess))
                    }
                }

                if (scenarios.size != numberOfScenarios)
                    logger.warn("Number of scenarios mismatch: specified $numberOfScenarios, but found ${scenarios.size}")

                scenarios
            }
        }

        private val regexAction by lazy {
            // Regex("""^(?<type>in|out)=(?<event>.*?)\[(?<values>[01]*)]$""")
            Regex("""^(in|out)=(.*?)\[([01]*)]$""")
        }

        private val regexOutputValues by lazy {
            Regex("""out=.*?\[([01]*)]""")
        }

        fun fromString(s: String, preprocess: Boolean = true): PositiveScenario {
            var lastOutputValues = Globals.INITIAL_OUTPUT_VALUES
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
                        is InputAction ->
                            ScenarioElement(first, OutputAction(null, lastOutputValues))
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

        fun fromTrace(
            trace: THE_Counterexample,
            // inputEvents: List<InputEvent>,
            // outputEvents: List<OutputEvent>,
            inputNames: List<String>,
            outputNames: List<String>,
        ): PositiveScenario {
            val elements = trace.nodes
                .map { node ->
                    node.states.single().values.associate { value ->
                        value.variable to value.content.toBoolean()
                    }
                }
                .zipWithNext { inputData, outputData ->
                    ScenarioElement(
                        inputAction = InputAction(
                            event = InputEvent("REQ"),
                            values = InputValues(inputNames.map { inputData.getValue(it) })
                        ),
                        outputAction = OutputAction(
                            event = OutputEvent("CNF"),
                            values = OutputValues(outputNames.map { outputData.getValue(it) })
                        )
                    )
                }
            return PositiveScenario(elements)
        }
    }
}
