package ru.ifmo.fbsat.core.scenario.negative

import ru.ifmo.fbsat.core.utils.sourceAutoGzip
import ru.ifmo.fbsat.core.utils.useLines
import java.io.File

data class Counterexample(
    val states: List<State>
) {
    /**
     * One-based index of loop-back state, or `null` if there is no loop.
     */
    val loopPosition: Int? = states.indexOfFirst { it.isLoop }.let { if (it == -1) null else it + 1 }

    init {
        if (loopPosition != null) {
            val loop = states[loopPosition - 1]
            val last = states.last()
            require(loop.variables == last.variables) {
                "Mismatch of variables in loopBack ($loop) and last ($last) states"
            }
        }

        // println("[*] Counterexample:")
        // states.forEachIndexed { index, state ->
        //     println("[${index + 1}/${states.size}] $state")
        // }
    }

    data class State(val name: String, val isLoop: Boolean, val variables: Map<String, String>) {
        fun getFirstTrue(names: List<String>): String? =
            names.firstOrNull { variables[it] == "TRUE" }

        fun getBooleanValues(names: List<String>): List<Boolean> =
            names.map {
                when (variables[it]) {
                    "TRUE" -> true
                    "FALSE" -> false
                    else -> error("Value of variable '$it' must be either 'TRUE' or 'FALSE', but encountered '${variables[it]}'")
                }
            }
    }

    override fun toString(): String {
        return "Counterexample(loopPosition=$loopPosition, states=$states)"
    }

    companion object {
        fun fromFile(file: File): List<Counterexample> {
            val ces: MutableList<Counterexample> = mutableListOf()
            var states: MutableList<State> = mutableListOf()
            var hasState = false
            var stateName = ""
            var isLoop = false
            var variables: MutableMap<String, String> = mutableMapOf()

            file.sourceAutoGzip().useLines { lines ->
                for (line in lines.map(String::trim)) {
                    when {
                        line.startsWith("Trace Description") -> {
                            // Add last state
                            if (hasState) {
                                states.add(State(stateName, isLoop, variables))
                            }
                            hasState = false
                            // Add counter-example
                            if (states.isNotEmpty()) {
                                ces.add(Counterexample(states))
                            }
                            // Reset states
                            states = mutableListOf()
                        }
                        line.startsWith("Trace Type") -> {
                            // Do nothing
                        }
                        line == "-- Loop starts here" -> {
                            // Add last state
                            if (hasState) {
                                states.add(State(stateName, isLoop, variables))
                            }
                            hasState = false
                            isLoop = true
                        }
                        line.startsWith("-> State") -> {
                            // Add last state
                            if (hasState) {
                                states.add(State(stateName, isLoop, variables))
                                // Reset isLoop only after state processing
                                isLoop = false
                            }
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
                if (hasState) {
                    states.add(State(stateName, isLoop, variables))
                }
                // Post-add counter-example
                if (states.isNotEmpty()) {
                    ces.add(Counterexample(states))
                }
            }

            return ces
        }
    }
}
