package ru.ifmo.fbsat.core.scenario2.positive

import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.automaton.OutputValues
import ru.ifmo.fbsat.core.scenario2.InputAction2
import ru.ifmo.fbsat.core.scenario2.OutputAction2
import ru.ifmo.fbsat.core.scenario2.Scenario2
import ru.ifmo.fbsat.core.scenario2.ScenarioElement2
import ru.ifmo.fbsat.core.scenario2.preprocessed
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.sourceAutoGzip
import ru.ifmo.fbsat.core.utils.toBooleanList
import ru.ifmo.fbsat.core.utils.useLines
import java.io.File

data class PositiveScenario2(
    override val elements: List<ScenarioElement2>
) : Scenario2 {
    companion object {
        fun fromFile(file: File, preprocess: Boolean = true): List<PositiveScenario2> {
            return file.sourceAutoGzip().useLines { lines ->
                val scenarios: MutableList<PositiveScenario2> = mutableListOf()
                var numberOfScenarios = 0

                for ((index, line) in lines.withIndex()) {
                    if (index == 0) {
                        numberOfScenarios = line.toInt()
                    } else {
                        scenarios.add(fromString(line, preprocess))
                    }
                }

                if (scenarios.size != numberOfScenarios)
                    log.warn("Number of scenarios mismatch: specified $numberOfScenarios, but found ${scenarios.size}")

                scenarios
            }
        }

        private val regexAction by lazy {
            Regex("""^(?<type>in|out)=(?<event>.*?)\[(?<values>[01]*)]$""")
        }

        fun fromString(s: String, preprocess: Boolean = true): PositiveScenario2 {
            var lastOutputValues = Globals.INITIAL_OUTPUT_VALUES
            val actions = s.splitToSequence(";")
                .map { it.trim() }
                .filter { it.isNotEmpty() }
                .map { regexAction.matchEntire(it) }
                .requireNoNulls()
                .map {
                    val type = it.groups["type"]!!.value
                    val event = it.groups["event"]!!.value
                    val values = it.groups["values"]!!.value
                    when (type) {
                        "in" -> InputAction2(InputEvent(event), InputValues(values.toBooleanList()))
                        "out" -> OutputAction2(OutputEvent(event), OutputValues(values.toBooleanList()))
                        else -> error("Unsupported action type '$type'")
                    }
                }
            val elements: MutableList<ScenarioElement2> = mutableListOf()
            var currentInputAction: InputAction2? = null
            val currentOutputActions: MutableList<OutputAction2> = mutableListOf()
            for (action in actions) {
                when (action) {
                    is InputAction2 -> {
                        if (currentInputAction != null) {
                            // Note: make a copy of `currentOutputActions`!
                            elements.add(ScenarioElement2(currentInputAction, currentOutputActions.toList()))
                        }
                        currentInputAction = action
                        currentOutputActions.clear()
                    }
                    is OutputAction2 -> {
                        if (currentInputAction == null) {
                            log.warn("Encountered OutputAction $action before any InputAction")
                        } else {
                            currentOutputActions.add(action)
                        }
                    }
                }
            }
            if (currentInputAction != null) {
                if (currentOutputActions.isEmpty()) {
                    log.warn("Ignoring trailing InputAction: $currentInputAction")
                } else {
                    // Note: make a copy of `currentOutputActions`!
                    elements.add(ScenarioElement2(currentInputAction, currentOutputActions.toList()))
                }
            }

            return if (preprocess)
                PositiveScenario2(elements.preprocessed)
            else
                PositiveScenario2(elements)
        }
    }
}
