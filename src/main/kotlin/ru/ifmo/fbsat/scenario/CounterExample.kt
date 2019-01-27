package ru.ifmo.fbsat.scenario

import java.io.File

class CounterExample(
    elements: List<ScenarioElement>,
    val loopPosition: Int?  // Note: 1-based
) : Scenario(elements, preprocess = false) {

    init {
        if (loopPosition != null)
            require(elements[loopPosition - 1] == elements.last()) {
                "elements[loopPosition = $loopPosition] = ${elements[loopPosition - 1]}  !=  last(${elements.size + 1}-th) = ${elements.last()}"
            }

        println("[.] Created CE(loop = $loopPosition)")
    }

    private enum class ParseState {
        BEGIN, STATE
    }

    companion object {
        data class State(val name: String, val isLoop: Boolean) {
            val variables: MutableMap<String, String> = mutableMapOf()
        }

        fun fromFile(
            file: File,
            inputEvents: List<String>,
            outputEvents: List<String>,
            inputNames: List<String>,
            outputNames: List<String>
        ): List<CounterExample> {
            val ces: MutableList<List<State>> = mutableListOf()
            var states: MutableList<State> = mutableListOf()
            var state: State? = null
            var isLoop = false

            file.bufferedReader().useLines { lines ->
                for (line in lines.map(String::trim)) {
                    when {
                        line == "Trace Description: LTL Counterexample" -> {
                            // Last-add state
                            state?.let { states.add(it) }
                            // Add ce
                            if (states.isNotEmpty())
                                ces.add(states)
                            // Reset states
                            states = mutableListOf()
                            state = null
                            isLoop = false
                        }
                        line == "Trace Type: Counterexample" -> {
                            // Do nothing
                        }
                        line == "-- Loop starts here" -> {
                            isLoop = true
                        }
                        line.startsWith("-> State") -> {
                            // Add state
                            state?.let { states.add(it) }
                            // Create state
                            state = State(line.substringAfter("-> State: ").substringBefore(" <-"), isLoop)
                            isLoop = false
                        }
                        else -> {
                            val (name, value) = line.split(" = ", limit = 2)
                            state!!.variables[name] = value
                        }
                    }
                }

                // Post-add state
                state?.let { states.add(it) }
                // Post-add ce
                if (states.isNotEmpty())
                    ces.add(states)
            }

            return ces.map { ce: List<State> ->
                val elements = ce.zipWithNext { a, b ->
                    val inputEvent = inputEvents.first { a.variables[it] == "TRUE" }
                    val inputValues = inputNames.joinToString("") {
                        when (a.variables.getOrElse(it) { a.variables["P.$it"] }) {
                            "TRUE" -> "1"
                            "FALSE" -> "0"
                            else -> throw IllegalStateException("Value of variable '$it' must be 'TRUE' or 'FALSE'")
                        }
                    }
                    val outputEvent = outputEvents.firstOrNull { b.variables["C.$it"] == "TRUE" }
                    val outputValues = outputNames.joinToString("") {
                        when (b.variables["C.$it"]) {
                            "TRUE" -> "1"
                            "FALSE" -> "0"
                            else -> throw IllegalStateException("Value of variable '$it' must be 'TRUE' or 'FALSE'")
                        }
                    }
                    ScenarioElement(inputEvent, inputValues, outputEvent, outputValues)
                }
                ce.indexOfFirst { it.isLoop }.let { loopPosition ->
                    if (loopPosition != -1) {
                        CounterExample(elements + elements[loopPosition], loopPosition + 1)
                    } else {
                        CounterExample(elements, null)
                    }
                }
            }
        }

        fun fromFile2(
            filename: String,
            inputEvents: List<String>,
            outputEvents: List<String>,
            inputNames: List<String>,
            outputNames: List<String>
        ): List<CounterExample> {
            val counterExamples: MutableList<CounterExample> = mutableListOf()
            lateinit var elements: MutableList<ScenarioElement>
            lateinit var variables: MutableMap<String, String>

            var parseState = ParseState.BEGIN
            var currentState = -1
            var loopPosition: Int? = null  // 1-based

            fun addElement() {
                if ("REQ" !in variables) {
                    println("variables: $variables")
                }
                val inputEvent = inputEvents.first { variables[it] == "TRUE" }
                val inputValues = inputNames.joinToString("") {
                    when (variables.getOrElse(it) { variables.getOrElse("C.$it") { variables["P.$it"] } }) {
                        "TRUE" -> "1"
                        "FALSE" -> "0"
                        else -> throw Exception("Value of variable '$it' must be 'TRUE' or 'FALSE' ($it :: ${variables[it]}, ${variables["C.$it"]}, ${variables["P.$it"]})")
                    }
                }
                val outputEvent = outputEvents.firstOrNull { variables["C.$it"] == "TRUE" }
                val outputValues = outputNames.joinToString("") {
                    when (variables["C.$it"]) {
                        "TRUE" -> "1"
                        "FALSE" -> "0"
                        else -> throw Exception("Value must be 'TRUE' or 'FALSE'")
                    }
                }
                val element = ScenarioElement(inputEvent, inputValues, outputEvent, outputValues)
                elements.add(element)
            }

            fun addCounterExample() {
                val counterExample = CounterExample(elements, loopPosition)
                counterExamples.add(counterExample)
                loopPosition = null
                parseState = ParseState.BEGIN
            }

            File(filename).bufferedReader().useLines { lines ->
                for (line in lines.map(String::trim)) {
                    when (parseState) {
                        ParseState.BEGIN -> {
                            when {
                                line.startsWith("Trace") -> {
                                    // Do nothing
                                }
                                line.startsWith("-> State") -> {
                                    elements = mutableListOf()
                                    variables = mutableMapOf()
                                    currentState = 1
                                    parseState = ParseState.STATE
                                }
                                else -> throw Exception("Could not parse '$line' (state = $parseState)")
                            }
                        }

                        ParseState.STATE -> {
                            val regexVariableDeclaration = Regex("""^(.+?)\s*=\s*(.+)$""")
                            val match = regexVariableDeclaration.matchEntire(line)
                            if (match != null) {
                                val (name, value) = match.destructured
                                variables[name] = value
                            } else {
                                if (variables.isNotEmpty()) {
                                    addElement()
                                    variables = mutableMapOf()
                                }

                                when {
                                    line.startsWith("-> State") -> currentState++
                                    line == "-- Loop starts here" -> loopPosition = currentState + 1
                                    line.startsWith("Trace Description") -> addCounterExample()
                                    else -> throw Exception("Could not parse '$line' (state = $parseState)")
                                }
                            }
                        }
                    }
                }
            }

            when (parseState) {
                ParseState.BEGIN -> {
                    throw Exception("The state after parsing must be ${ParseState.STATE}")
                }
                ParseState.STATE -> {
                    println("Post adding...")
                    addElement()
                    addCounterExample()
                    println("Last: ${elements.last()}")
                }
            }

            return counterExamples
        }
    }

    override fun toString(): String {
        return "CounterExample(loopPosition=$loopPosition, elements=$elements)"
    }
}
