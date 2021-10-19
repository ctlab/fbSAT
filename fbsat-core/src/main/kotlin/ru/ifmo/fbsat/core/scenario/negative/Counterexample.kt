package ru.ifmo.fbsat.core.scenario.negative

import kotlinx.serialization.Serializable
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString
import nl.adaptivity.xmlutil.XmlDeclMode
import nl.adaptivity.xmlutil.serialization.XML
import nl.adaptivity.xmlutil.serialization.XmlElement
import nl.adaptivity.xmlutil.serialization.XmlSerialName
import nl.adaptivity.xmlutil.serialization.XmlValue
import ru.ifmo.fbsat.core.utils.sourceAutoGzip
import ru.ifmo.fbsat.core.utils.useLines
import java.io.File

data class Counterexample(
    val states: List<ExecutionState>,
    /**
     * One-based index of loop-back state, or `null` if there is no loop.
     */
    val loopPosition: Int?,
) {
    val loopBack: ExecutionState? = loopPosition?.let { l -> states[l - 1] }

    init {
        if (loopPosition != null) {
            val loop = states[loopPosition - 1]
            val last = states.last()
            require(loop.data == last.data) {
                "Mismatch of variables in loopBack ($loop) and last ($last) states"
            }
        }
    }

    data class ExecutionState(
        val data: Map<String, String>,
    ) {
        fun getFirstTrue(names: List<String>): String? =
            names.firstOrNull { data[it] == "TRUE" }

        fun getBooleanValues(names: List<String>): List<Boolean> =
            names.map {
                when (val v = data[it]) {
                    "TRUE" -> true
                    "FALSE" -> false
                    else -> error("Value of boolean variable '$it' must be either 'TRUE' or 'FALSE', but encountered '$v'")
                }
            }
    }

    override fun toString(): String {
        return "Counterexample(loopPosition=$loopPosition, states=$states)"
    }

    companion object {
        fun from(ceXML: CounterexampleXML): Counterexample {
            val states = ceXML.nodes.map { it.state }.map {
                ExecutionState(it.values.associate { v -> v.variable to v.content })
            }
            val loopPosition: Int? = if (ceXML.loops.isBlank()) {
                null
            } else {
                // Note: we assume that only the first loop is essential, so we drop the others
                ceXML.loops.split(",").map { it.trim().toInt() }.first()
            }
            return Counterexample(states, loopPosition)
        }

        fun fromSmvout(lines: Sequence<String>): List<Counterexample> {
            val ces: MutableList<Counterexample> = mutableListOf()
            var states: MutableList<ExecutionState> = mutableListOf()
            var hasState = false
            var loopPosition: Int? = null
            var data: MutableMap<String, String> = mutableMapOf()

            for (line in lines.map(String::trim)) {
                when {
                    line.startsWith("Trace Description") -> {
                        // Add last state
                        if (hasState) {
                            states.add(ExecutionState(data))
                        }
                        hasState = false
                        // Add counter-example
                        if (states.isNotEmpty()) {
                            ces.add(Counterexample(states, loopPosition))
                        }
                        // Reset states
                        states = mutableListOf()
                        loopPosition = null
                    }
                    line.startsWith("Trace Type") -> {
                        // Do nothing
                    }
                    line == "-- Loop starts here" -> {
                        // Add last state
                        if (hasState) {
                            states.add(ExecutionState(data))
                        }
                        hasState = false
                        // Remember only the first loopPosition
                        if (loopPosition == null) {
                            loopPosition = states.size + 1
                        }
                    }
                    line.startsWith("-> State") -> {
                        // Add last state
                        if (hasState) {
                            states.add(ExecutionState(data))
                        }
                        // New state
                        hasState = true
                        data = mutableMapOf()
                    }
                    else -> {
                        val (name, value) = line.split(" = ", limit = 2)
                        // Cut dot-prefix in name (e.g. "C." - controller var, or "P." - plant var)
                        // FIXME: is it really OK to cut dot-prefixes?
                        data[name.substringAfter('.').substringAfter("$")] = value
                    }
                }
            }

            // Post-add state
            if (hasState) {
                states.add(ExecutionState(data))
            }
            // Post-add counter-example
            if (states.isNotEmpty()) {
                ces.add(Counterexample(states, loopPosition))
            }

            return ces
        }

        fun fromSmvout(file: File): List<Counterexample> {
            return file.sourceAutoGzip().useLines { lines -> fromSmvout(lines) }
        }

        fun fromXml(file: File): List<Counterexample> {
            return CounterexampleXML.from(file).map(::from)
        }

        fun from(file: File): List<Counterexample> {
            return when (file.extension) {
                "xml" -> fromXml(file)
                "smvout" -> fromSmvout(file)
                // FIXME: for backward-compatibility we have to default to smvout format
                else -> fromSmvout(file)
            }
        }
    }
}

@Serializable
@XmlSerialName("counter-example", "", "")
data class CounterexampleXML(
    val type: Int,
    val id: Int,
    val desc: String,
    val nodes: List<Node>,
    @XmlElement(true)
    val loops: String,
) {
    @Serializable
    @XmlSerialName("node", "", "")
    data class Node(
        val state: State,
    )

    @Serializable
    @XmlSerialName("state", "", "")
    data class State(
        val id: Int,
        val values: List<Value>,
    )

    @Serializable
    @XmlSerialName("value", "", "")
    data class Value(
        val variable: String,
        @XmlValue(true)
        val content: String,
    )

    companion object {
        private val defaultFormat = XML()

        fun from(s: String, format: XML = defaultFormat): CounterexampleXML {
            return format.decodeFromString(s)
        }

        fun from(file: File, format: XML = defaultFormat): List<CounterexampleXML> {
            val lines = file.readLines()
            val headerLines = lines.mapIndexedNotNull { index, s ->
                if (s.startsWith("<?xml")) index else null
            }
            return (headerLines + lines.size)
                .zipWithNext { a, b ->
                    lines.subList(a, b).joinToString("\n")
                }
                .map { from(it, format) }
        }
    }
}

fun main() {
    val s = """
        <?xml version="1.0" encoding="UTF-8"?>
        <counter-example type="1" id="1" desc="Simulation Trace" >
            <node>
                <state id="1">
                    <value variable="cat">FALSE</value>
                    <value variable="dog">FALSE</value>
                </state>
            </node>
            <node>
                <state id="2">
                    <value variable="cat">FALSE</value>
                    <value variable="dog">TRUE</value>
                </state>
            </node>
            <node>
                <state id="3">
                    <value variable="cat">FALSE</value>
                    <value variable="dog">TRUE</value>
                </state>
            </node>
            <loops> 2 ,3 </loops>
        </counter-example>
    """.trimIndent()

    val xml = XML {
        indentString = "    "
        xmlDeclMode = XmlDeclMode.Charset
    }

    val ce = CounterexampleXML.from(s, xml)
    println("ce = $ce")

    val serialized = xml.encodeToString(ce)
    println("serialized:\n$serialized")
}
