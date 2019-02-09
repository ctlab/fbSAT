package ru.ifmo.fbsat.scenario

import java.io.File

class CounterExample(
    val states: List<State>
) {
    /**
     * One-based index of loop-back state, or `null` if there is no loop.
     */
    val loopPosition: Int? = states.indexOfFirst { it.isLoop }.let { if (it == -1) null else it + 1 }

    init {
        if (loopPosition != null)
            require(states[loopPosition - 1].variables == states.last().variables) {
                println("[DEBUG] loop = ${states[loopPosition-1]}")
                println("[DEBUG] last = ${states.last()}")
                "Loop-back state must be equal to last state in counter-example"
            }

        println("[*] CounterExample:")
        states.forEachIndexed { index, state ->
            println("[${index + 1}/${states.size}] $state")
        }
    }

    data class State(val name: String, val isLoop: Boolean, val variables: Map<String, String>) {
        fun getFirstTrue(names: List<String>) = names.firstOrNull { variables[it] == "TRUE" }

        fun getBooleanString(names: List<String>) = names.joinToString("") {
            when (variables[it]) {
                "TRUE" -> "1"
                "FALSE" -> "0"
                else -> throw IllegalStateException("Value of variable '$it' must be 'TRUE' or 'FALSE'")
            }
        }
    }

    override fun toString(): String {
        return "CounterExample(loopPosition=$loopPosition, states=$states)"
    }

    companion object {
        fun fromFile(file: File): List<CounterExample> {
            val ces = mutableListOf<CounterExample>()
            var states = mutableListOf<State>()
            var hasState = false
            var stateName = ""
            var isLoop = false
            var variables = mutableMapOf<String, String>()

            file.bufferedReader().useLines { lines ->
                for (line in lines.map(String::trim)) {
                    when {
                        line == "Trace Description: LTL Counterexample" -> {
                            // Add last state
                            if (hasState)
                                states.add(State(stateName, isLoop, variables))
                            hasState = false
                            // Add counter-example
                            if (states.isNotEmpty())
                                ces.add(CounterExample(states))
                            // Reset states
                            states = mutableListOf()
                        }
                        line == "Trace Type: Counterexample" -> {
                            // Do nothing
                        }
                        line == "-- Loop starts here" -> {
                            // Add last state
                            if (hasState)
                                states.add(State(stateName, isLoop, variables))
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
                if (hasState)
                    states.add(State(stateName, isLoop, variables))
                // Post-add counter-example
                if (states.isNotEmpty())
                    ces.add(CounterExample(states))
            }

            return ces
        }
    }
}
