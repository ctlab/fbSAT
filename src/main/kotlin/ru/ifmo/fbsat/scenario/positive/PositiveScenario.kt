package ru.ifmo.fbsat.scenario.positive

import ru.ifmo.fbsat.scenario.Scenario
import ru.ifmo.fbsat.scenario.ScenarioElement
import ru.ifmo.fbsat.scenario.preprocessed
import java.io.File

class PositiveScenario(
    elements: List<ScenarioElement>,
    preprocess: Boolean = true
) : Scenario {
    override val elements: List<ScenarioElement>

    init {
        if (preprocess && elements.isNotEmpty())
            this.elements = elements.preprocessed
        else
            this.elements = elements
    }

    override fun toString(): String {
        return "PositiveScenario(elements=$elements)"
    }

    companion object {
        fun fromFile(file: File, preprocess: Boolean = true): List<PositiveScenario> {
            file.bufferedReader().useLines { lines ->
                val scenarios: MutableList<PositiveScenario> = mutableListOf()
                var numberOfScenarios = 0

                for ((index, line) in lines.withIndex()) {
                    if (index == 0) numberOfScenarios = line.toInt()
                    else scenarios.add(
                        fromString(
                            line,
                            preprocess
                        )
                    )
                }

                if (scenarios.size != numberOfScenarios) {
                    System.err.println(
                        "[!] Number of scenarios mismatch: specified $numberOfScenarios, but found ${scenarios.size}"
                    )
                }

                return scenarios
            }
        }

        private val regexAction by lazy {
            // Regex("""^(?<type>in|out)=(?<event>.*?)\[(?<values>[01]*)]$""")
            Regex("""^(in|out)=(.*?)\[([01]*)]$""")
        }

        private val regexOutputValues by lazy {
            Regex("""out=.*?\[([01]*)]""")
        }

        private class Action(val type: String, val event: String, val values: String)

        private fun fromString(s: String, preprocess: Boolean = true): PositiveScenario {
            val elements: MutableList<ScenarioElement> = mutableListOf()

            var lastOutputValues = "0".repeat(regexOutputValues.find(s)!!.groups[1]!!.value.length)

            val actions = s.splitToSequence(";")
                .map(String::trim)
                .filter(String::isNotEmpty)
                .map { regexAction.matchEntire(it) }
                .requireNoNulls()
                .map {
                    val (type, event, values) = it.destructured
                    Action(type, event, values)
                }

            for ((first, second) in actions.zipWithNext().filter { it.first.type == "in" }) {
                when (second.type) {
                    "in" -> elements.add(
                        ScenarioElement(
                            first.event,
                            first.values,
                            null,
                            lastOutputValues
                        )
                    )
                    "out" -> {
                        elements.add(
                            ScenarioElement(
                                first.event,
                                first.values,
                                second.event,
                                second.values
                            )
                        )
                        lastOutputValues = second.values
                    }
                    else -> throw UnsupportedOperationException("Unsupported action type ${second.type}")
                }
            }

            return PositiveScenario(elements, preprocess = preprocess)
        }
    }
}
