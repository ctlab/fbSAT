package ru.ifmo.fbsat.automaton

import ru.ifmo.fbsat.scenario.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.utils.LazyCache
import ru.ifmo.fbsat.utils.toBooleanArray
import ru.ifmo.fbsat.utils.toBooleanString
import java.io.File

class Automaton(
    /**
     * Zero-based list of input events.
     */
    val inputEvents: List<String>,
    /**
     * Zero-based list of output events.
     */
    val outputEvents: List<String>,
    val inputNames: List<String>,
    val outputNames: List<String>
) {
    private var _states: MutableMap<Int, State> = mutableMapOf()
    private val lazyCache = LazyCache()

    val states: Set<State> by lazyCache {
        _states.values.toSet()
    }
    val initialState: State
        get() = states.first()
    val numberOfStates: Int
        get() = states.size
    val numberOfTransitions: Int
        get() = states.sumBy { it.transitions.size }
    val totalGuardsSize: Int
        get() = states.flatMap { it.transitions }.sumBy { it.guard.size }

    constructor(scenarioTree: ScenarioTree) : this(
        scenarioTree.inputEvents,
        scenarioTree.outputEvents,
        scenarioTree.inputNames,
        scenarioTree.outputNames
    )

    fun addState(id: Int, outputEvent: String, algorithm: Algorithm) {
        require(id !in _states) { "Automaton already has state '$id'" }
        _states[id] = State(id, outputEvent, algorithm)
        lazyCache.invalidate()
    }

    fun getState(id: Int): State {
        return _states[id]!!
    }

    fun addTransition(sourceId: Int, destinationId: Int, inputEvent: String, guard: Guard) {
        val source = getState(sourceId)
        val destination = getState(destinationId)
        source.addTransition(destination, inputEvent, guard)
    }

    inner class State(
        val id: Int,
        val outputEvent: String,
        val algorithm: Algorithm
    ) {
        private val _transitions: MutableList<Transition> = mutableListOf()
        val transitions: List<Transition> = _transitions

        fun addTransition(destination: State, inputEvent: String, guard: Guard) {
            _transitions.add(Transition(destination, inputEvent, guard))
        }

        inner class Transition(
            val destination: State,
            val inputEvent: String,
            val guard: Guard
        ) {
            val source: State = this@State
            val k: Int = this@State.transitions.size + 1  // Note: 1-based

            fun eval(inputValues: String): Boolean {
                return guard.eval(inputValues.toBooleanArray())
            }

            fun toSimpleString(): String {
                return "${source.id} to ${destination.id} on $inputEvent if ${guard.toSimpleString()}"
            }

            fun toGraphvizString(): String {
                return "${source.id} -> ${destination.id} [label=\"$k:$inputEvent/${guard.toGraphvizString()}\"]"
            }

            fun toSmvString(): String {
                TODO()
            }

            override fun toString(): String {
                return "Transition(k=$k, source=${source.id}, destination=${destination.id}, $inputEvent=$inputEvent, guard=$guard)"
            }
        }

        fun eval(outputValues: String): String {
            return algorithm.eval(outputValues.toBooleanArray()).toBooleanString()
        }

        fun go(inputEvent: String, inputValues: String, outputValues: String): GoResult {
            for (transition in transitions) {
                if (transition.inputEvent == inputEvent && transition.eval(inputValues)) {
                    val destination = transition.destination
                    val outputEvent = destination.outputEvent
                    val newValues = destination.eval(outputValues)
                    return GoResult(destination, outputEvent, newValues)
                }
            }
            return GoResult(this, null, outputValues)
        }

        fun toSimpleString(): String {
            return "$id/$outputEvent(${algorithm.toSimpleString()})"
        }

        fun toGraphvizString(): String {
            algorithm as BinaryAlgorithm
            return "$id [label=\"<p>$id|$outputEvent|{0:${algorithm.algorithm0.toBooleanString()}|1:${algorithm.algorithm1.toBooleanString()}}\" shape=Mrecord]"
        }

        fun toSmvString(): String {
            return "s$id"
        }

        override fun toString(): String {
            return "State(id=$id, outputEvent=$outputEvent, algorithm=$algorithm, transitions=${transitions.map { it.destination.id }})"
        }
    }

    data class GoResult(val destination: State, val outputEvent: String?, val newValues: String)

    fun go(source: State, inputEvent: String, inputValues: String, outputValues: String): GoResult {
        return source.go(inputEvent, inputValues, outputValues)
    }

    fun verify(scenarioTree: ScenarioTree): Boolean {
        var ok = true

        outer@ for ((i, scenario) in scenarioTree.scenarios.withIndex()) {
            var currentState = this.initialState
            var currentValues = scenarioTree.rootElement!!.outputValues

            for ((j, element) in scenario.elements.withIndex()) {
                val inputEvent = element.inputEvent
                val inputValues = element.inputValues
                val (newState, outputEvent, newValues) = go(currentState, inputEvent, inputValues, currentValues)
                // println("[${i+1}::${j+1}] ${currentState.id} -> ${newState.id} / $outputEvent [$newValues]")

                if (outputEvent != element.outputEvent) {
                    println("[${i + 1}::${j + 1}] Incorrect outputEvent($outputEvent) != element.outputEvent(${element.outputEvent})  [currentState = ${currentState.id}, inputEvent = $inputEvent, inputValues = $inputValues, currentValues = $currentValues]")
                    ok = false
                }
                // else println("[${i + 1}:${j + 1}] Correct outputEvent($outputEvent)")
                if (newValues != element.outputValues) {
                    println("[${i + 1}::${j + 1}] Incorrect newValues($newValues) != element.outputValues(${element.outputValues})  [currentState = ${currentState.id}, inputEvent = $inputEvent, inputValues = $inputValues, currentValues = $currentValues]")
                    ok = false
                }
                // else println("[${i + 1}:${j + 1}] Correct newValues($newValues)")

                currentState = newState
                currentValues = newValues
            }
        }

        return ok
    }

    fun verify(negativeScenarioTree: NegativeScenarioTree): Boolean {
        var ok = true

        for ((i, counterExample) in negativeScenarioTree.counterExamples.withIndex()) {
            val satisfyingStates = MutableList<State?>(counterExample.elements.size) { null }
            satisfyingStates[0] = this.initialState
            // println("[${i + 1}::1/${counterExample.elements.size}] ${negativeScenarioTree.rootElement} satisfied by ${this.initialState}")

            var currentState = this.initialState
            var currentValues = negativeScenarioTree.rootElement!!.outputValues

            for ((j, element) in counterExample.elements.withIndex().drop(1)) {
                // j -- 0-based index of CE-element (CE-state)
                val inputEvent = element.inputEvent
                val inputValues = element.inputValues
                val (newState, outputEvent, newValues) = go(currentState, inputEvent, inputValues, currentValues)

                if (outputEvent == element.outputEvent && newValues == element.outputValues) {
                    // println("[${i + 1}::${j + 1}/${counterExample.elements.size}] $element satisfied by $newState")
                    satisfyingStates[j] = newState
                } else {
                    // println("[${i + 1}::${j + 1}/${counterExample.elements.size}] $element not satisfied (newState=$newState, newValues=$newValues)")
                    break
                }

                currentState = newState
                currentValues = newValues
            }

            println("[CE::${i + 1}] Satisfying states: [${satisfyingStates.map { it?.id }.withIndex().joinToString(" ") { (j, x) -> if (j + 1 == counterExample.loopPosition) "«$x»" else "$x" }}] (loop = ${counterExample.loopPosition})")

            if (counterExample.loopPosition != null) {
                val loop = satisfyingStates[counterExample.loopPosition]
                val last = satisfyingStates.last()
                if (loop != null && last != null) {
                    if (last == loop) {
                        println("[!] Counterexample #${i + 1} is satisfied (last==loop)\n>>> loopPosition = ${counterExample.loopPosition}\n>>> loop = $loop\n>>> last = $last")
                        ok = false
                    }
                }
            } else if (satisfyingStates.last() != null) {
                println("[!] Terminal in counterexample #${i + 1} is satisfied")
            }
        }

        return ok
    }

    fun dump(dir: File, name: String) {
        _dumpGv(dir.resolve("$name.gv"))
        _dumpSmv(dir.resolve("$name.smv"))
    }

    @Suppress("FunctionName")
    fun _dumpGv(file: File) {
        file.printWriter().use {
            it.println(this.toGraphvizString())
        }
        Runtime.getRuntime().exec("dot -Tpdf -O $file")
        Runtime.getRuntime().exec("dot -Tpng -O $file")
    }

    @Suppress("FunctionName")
    fun _dumpSmv(file: File) {
        file.printWriter().use {
            it.println(this.toSmvString())
        }
    }

    fun pprint() {
        println(toSimpleString().prependIndent("  "))
    }

    fun toSimpleString(): String {
        val lines: MutableList<String> = mutableListOf()
        for (state in states) {
            if (state.transitions.isNotEmpty()) {
                lines.add("┌─${state.toSimpleString()}")
                for (transition in state.transitions.dropLast(1)) {
                    lines.add("├──${transition.toSimpleString()}")
                }
                lines.add("└──${state.transitions.last().toSimpleString()}")
            } else {
                lines.add("──${state.toSimpleString()}")
            }
        }
        return lines.joinToString("\n")
    }

    fun toGraphvizString(): String {
        val setupBlock = sequenceOf("graph", "node", "edge")
            .map { "    $it [fontname=\"Source Code Pro,monospace\" fontsize=\"12\"]" }
            .joinToString("\n")

        // TODO: fix multiline string
        val nodesBlock = """
        |    // States
        |    { node [margin="0.05,0.01"]
        |    ${states.joinToString("\n", prefix = "  ") { it.toGraphvizString() }}
        |    }
        """.trimMargin()

        val transitionsBlock = states
            .flatMap { it.transitions }
            .joinToString("\n", prefix = "    ") { it.toGraphvizString() }

        return """
            digraph {
            $setupBlock
            $nodesBlock
            $transitionsBlock
            }
        """.trimIndent()
    }

    fun toSmvString(): String {
        val module = "CONTROL(${inputEvents.joinToString(",")}, ${inputNames.joinToString(",")})"

        val definitions: MutableMap<String, String> = mutableMapOf()
        definitions["_state"] = "{${states.joinToString(", ") { it.toSmvString() }}}"
        for (outputEvent in outputEvents.filter { it != "INITO" })
            definitions[outputEvent] = "boolean"
        for (outputName in outputNames)
            definitions[outputName] = "boolean"

        fun buildCase(cases: List<Pair<String, String>>, default: String): String {
            var s = "case\n"
            for ((lhs, rhs) in cases)
                s += "    $lhs : $rhs;\n"
            //if (default != null)
            s += "    TRUE : $default;\n"
            return s + "esac"
        }

        val declarations: MutableMap<String, Pair<String, String>> = mutableMapOf()  // {name: (init, next)}

        // State declarations
        val stateNextCases = states
            .flatMap { it.transitions }
            .map { "_state=${it.source.toSmvString()} & ${it.inputEvent} & (${it.guard.toSmvString()})" to it.destination.toSmvString() }
        declarations["_state"] = initialState.toSmvString() to buildCase(stateNextCases, default = "_state")

        // Output events declarations
        for (outputEvent in outputEvents.filter { it != "INITO" }) {
            val outputEventNextCases = states
                .flatMap { it.transitions }
                .filter { it.destination.outputEvent == outputEvent }
                .map { "_state=${it.source.toSmvString()} & ${it.inputEvent} & (${it.guard.toSmvString()})" to "TRUE" }

            declarations[outputEvent] = "FALSE" to buildCase(outputEventNextCases, default = "FALSE")
        }

        // Output variables declarations
        for (z in 1..outputNames.size) {
            val outputName = outputNames[z - 1]
            val outputVariableNextCases = states
                .flatMap { it.transitions }
                .flatMap {
                    val guard = "_state=${it.source.toSmvString()} & ${it.inputEvent} & (${it.guard.toSmvString()})"
                    val algo = it.destination.algorithm as BinaryAlgorithm
                    listOf(
                        "$guard & !$outputName" to algo.algorithm0[z - 1].toString().toUpperCase(),
                        "$guard & $outputName" to algo.algorithm1[z - 1].toString().toUpperCase()
                    )
                }
            declarations[outputName] = "FALSE" to buildCase(outputVariableNextCases, default = outputName)
        }

        val varBlock = definitions
            .map { (name, def) -> "$name : $def;" }
            .joinToString("\n")
            .prependIndent()

        val assignBlock = declarations
            .map { (name, declaration) -> "init($name) := ${declaration.first};\nnext($name) := ${declaration.second};" }
            .joinToString("\n\n")
            .prependIndent()

        return "MODULE $module\nVAR\n$varBlock\nASSIGN\n$assignBlock"
    }

    override fun toString(): String {
        return "Automaton(numberOfStates=$numberOfStates, numberOfTransitions=$numberOfTransitions, totalGuardsSize=$totalGuardsSize, inputEvents=$inputEvents, outputEvents=$outputEvents, inputNames=$inputNames, outputNames=$outputNames)"
    }

    companion object
}
