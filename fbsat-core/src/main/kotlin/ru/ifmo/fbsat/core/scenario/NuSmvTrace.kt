package ru.ifmo.fbsat.core.scenario

import ru.ifmo.fbsat.core.utils.sourceAutoGzip
import ru.ifmo.fbsat.core.utils.useLines
import java.io.File

data class NuSmvTrace(
    val states: List<State>
) {
    data class State(val name: String, val variables: Map<String, String>) {
        fun getFirstTrue(names: List<String>): String? =
            names.firstOrNull { variables[it] == "TRUE" }

        fun getBooleanValues(names: List<String>): List<Boolean> =
            names.map {
                when (variables[it]) {
                    "TRUE" -> true
                    "FALSE" -> false
                    else -> error("Value of variable '$it' must be either 'TRUE' or 'FALSE'")
                }
            }
    }

    companion object {
        fun fromFile(file: File): List<NuSmvTrace> {
            val traces: MutableList<NuSmvTrace> = mutableListOf()
            var states: MutableList<State> = mutableListOf()
            var hasState = false
            var stateName = ""
            var variables: MutableMap<String, String> = mutableMapOf()

            file.sourceAutoGzip().useLines { lines ->
                for (line in lines.map(String::trim)) {
                    when {
                        line.startsWith("Trace Description") -> {
                            // Add last state
                            if (hasState)
                                states.add(State(stateName, variables))
                            hasState = false
                            // Add counter-example
                            if (states.isNotEmpty())
                                traces.add(NuSmvTrace(states))
                            // Reset states
                            states = mutableListOf()
                        }
                        line.startsWith("Trace Type") -> {
                            // Do nothing
                        }
                        line.startsWith("-> State") -> {
                            // Add last state
                            if (hasState)
                                states.add(State(stateName, variables))
                            // New state
                            hasState = true
                            stateName = line.substringAfter("-> State: ").substringBefore(" <-")
                            variables = mutableMapOf()
                        }
                        else -> {
                            val (name, value) = line.split(" = ", limit = 2)
                            // Cut dot-prefix in name (e.g. "C." - controller var, or "P." - plant var)
                            variables[name.substringAfter('.')] = value
                        }
                    }
                }

                // Post-add state
                if (hasState)
                    states.add(State(stateName, variables))
                // Post-add counter-example
                if (states.isNotEmpty())
                    traces.add(NuSmvTrace(states))
            }

            return traces
        }
    }
}
