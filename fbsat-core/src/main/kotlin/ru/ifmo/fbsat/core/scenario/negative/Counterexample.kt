package ru.ifmo.fbsat.core.scenario.negative

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import nl.adaptivity.xmlutil.serialization.XML
import nl.adaptivity.xmlutil.serialization.XmlElement
import nl.adaptivity.xmlutil.serialization.XmlSerialName
import nl.adaptivity.xmlutil.serialization.XmlValue
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
                            variables[name.substringAfter('.').substringAfter("$")] = value
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

@Serializable
@XmlSerialName("counter-example", namespace = "", prefix = "")
data class THE_Counterexample(
    val type: Int,
    val id: Int,
    @XmlSerialName("desc", namespace = "", prefix = "")
    val description: String,
    @XmlSerialName("node", namespace = "", prefix = "")
    val nodes: List<THE_Node>,
    // @XmlSerialName("loops", namespace = "", prefix = "")
    // val loops: THE_Loops
    @XmlElement(true)
    val loops: String
) {
    @Serializable
    @XmlSerialName("node", namespace = "", prefix = "")
    data class THE_Node(
        @XmlSerialName("state", namespace = "", prefix = "")
        val states: List<THE_State>
    ) {
        @Serializable
        @XmlSerialName("state", namespace = "", prefix = "")
        data class THE_State(
            val id: Int,
            @SerialName("value")
            val values: List<THE_Value>
        ) {
            @Serializable
            @XmlSerialName("value", namespace = "", prefix = "")
            data class THE_Value(
                val variable: String,
                @XmlValue(true)
                val content: String
            )
        }
    }

    @Serializable
    @XmlSerialName("loops", namespace = "", prefix = "")
    data class THE_Loops(
        @XmlValue(true)
        val loops: String
    )
}

fun counterexampleFromString(s: String): THE_Counterexample {
    val xml = XML()
    return xml.parse(THE_Counterexample.serializer(), s)
}

fun readCounterexamplesFromFile(file: File): List<THE_Counterexample> {
    val xmlString = file.readText()
        .replace("<loops> </loops>", "<loops/>")
    val lines = xmlString.lines()
    val headerLines = lines.mapIndexedNotNull { index, s ->
        if (s.startsWith("<?xml")) index else null
    }.toList()
    return (headerLines + lines.size)
        .zipWithNext { a, b ->
            lines.subList(a, b).joinToString("\n")
        }
        .map(::counterexampleFromString)
        .also {
            check(it.size == xmlString.lineSequence().count { line -> line.contains("?xml") })
        }
}
