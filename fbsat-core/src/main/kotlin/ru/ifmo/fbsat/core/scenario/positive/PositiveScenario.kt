package ru.ifmo.fbsat.core.scenario.positive

import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.Scenario
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.preprocessed
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.sourceAutoGzip
import ru.ifmo.fbsat.core.utils.toBooleanList
import ru.ifmo.fbsat.core.utils.useLines
import java.io.File

data class PositiveScenario(
    override val elements: List<ScenarioElement>
) : Scenario {
    companion object {
        fun fromFile(file: File, preprocess: Boolean = true): List<PositiveScenario> {
            return file.sourceAutoGzip().useLines { lines ->
                val scenarios: MutableList<PositiveScenario> = mutableListOf()
                var numberOfScenarios = 0

                for ((index, line) in lines.withIndex()) {
                    if (index == 0) {
                        numberOfScenarios = line.toInt()
                    } else {
                        scenarios.add(
                            fromString(
                                line,
                                preprocess
                            )
                        )
                    }
                }

                if (scenarios.size != numberOfScenarios)
                    log.warn("Number of scenarios mismatch: specified $numberOfScenarios, but found ${scenarios.size}")

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
                            InputEvent(
                                event
                            ), InputValues(values.toBooleanList())
                        )
                        "out" -> OutputAction(
                            OutputEvent(
                                event
                            ), OutputValues(values.toBooleanList())
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
                            ScenarioElement(first,
                                OutputAction(null, lastOutputValues)
                            )
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
    }
}
