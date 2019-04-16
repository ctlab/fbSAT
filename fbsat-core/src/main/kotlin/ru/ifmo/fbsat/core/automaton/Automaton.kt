package ru.ifmo.fbsat.core.automaton

import org.redundent.kotlin.xml.PrintOptions
import org.redundent.kotlin.xml.xml
import ru.ifmo.fbsat.core.scenario.Scenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.utils.LazyCache
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.random
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.toBooleanArray
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
    /**
     * List of input variables names.
     */
    val inputNames: List<String>,
    /**
     * List of output variables names.
     */
    val outputNames: List<String>
) {
    private var _states: MutableMap<Int, State> = mutableMapOf()
    private val lazyCache = LazyCache()

    /**
     * Automaton states.
     */
    val states: Collection<State> by lazyCache {
        _states.values
    }
    /**
     * Initial automaton state.
     */
    val initialState: State
        // get() = states.first()
        get() = _states[1]!!
    /**
     * Number of automaton states **C**.
     */
    val numberOfStates: Int
        get() = states.size
    /**
     * Maximal guard size **P**.
     */
    val maxGuardSize: Int
        get() = transitions.map { it.guard.size }.max()!!
    /**
     * Automaton transitions.
     */
    val transitions: Collection<State.Transition> by lazyCache {
        states.flatMap { it.transitions }
    }
    /**
     * Number of automaton transitions **T**.
     */
    val numberOfTransitions: Int
        get() = transitions.size
    /**
     * Total guards size **N**.
     */
    val totalGuardsSize: Int
        get() = transitions.sumBy { it.guard.size }

    fun getN(): Int {
        // return totalGuardsSize + states.sumBy { state ->
        //     with(state.algorithm as BinaryAlgorithm) {
        //         algorithm0.count { it } + algorithm1.count { !it }
        //     }
        // }
        return totalGuardsSize
    }

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
            val k: Int = this@State.transitions.size + 1 // Note: 1-based

            fun eval(inputValues: String): Boolean {
                return guard.eval(inputValues.toBooleanArray())
            }

            fun toSimpleString(): String {
                return "${source.id} to ${destination.id} on $inputEvent if ${guard.toSimpleString()}"
            }

            fun toGraphvizString(): String {
                return "${source.id} -> ${destination.id} [label=\"$k:$inputEvent/${guard.toGraphvizString()}\"]"
            }

            fun toFbtString(): String {
                return "$inputEvent&${guard.toFbtString()}"
            }

            fun toSmvString(): String {
                return "_state=${source.toSmvString()} & $inputEvent & (${guard.toSmvString()})"
            }

            override fun toString(): String {
                return "Transition(k=$k, source=${source.id}, destination=${destination.id}, $inputEvent=$inputEvent, guard=$guard)"
            }
        }

        fun eval(outputValues: String): String {
            return algorithm.eval(outputValues.toBooleanArray()).toBinaryString()
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
            val X = outputNames.size

            val vs = (1..X).mapNotNull { x ->
                val name = outputNames[x - 1]
                val d0 = algorithm.algorithm0[x - 1]
                val d1 = algorithm.algorithm1[x - 1]
                when (d0 to d1) {
                    true to true -> """<TR><TD align="left">$name &rarr; 1</TD></TR>"""
                    false to false -> """<TR><TD align="left">$name &rarr; 0</TD></TR>"""
                    true to false -> """<TR><TD align="left">$name flip</TD></TR>"""
                    false to true -> null
                    else -> error("Bad combination of algorithms ${d0 to d1}")
                }
            }.joinToString("\n")

            val tableBody = """
                <TR><TD align="center">$id / $outputEvent</TD></TR>
                <HR/>
                %s
            """.trimIndent().format(vs)

            val html = """
                <TABLE style="rounded" cellborder="0" cellspacing="0">
                %s
                </TABLE>
            """.trimIndent().format(tableBody.prependIndent())

            return "$id [label=<\n${html.prependIndent()}> shape=plaintext]"
        }

        fun toFbtString(): String {
            return "s$id"
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

    /**
     * Evaluate given [scenario].
     *
     * @return list of satisfying automaton states (i.e. states, by which each scenario element is satisfied).
     */
    private fun eval(scenario: Scenario): List<State?> {
        val satisfyingStates = Array<State?>(scenario.elements.size) { null }

        var currentState = initialState
        var currentValues = "0".repeat(outputNames.size)

        for ((j, element) in scenario.elements.withIndex()) {
            val inputEvent = element.inputEvent
            val inputValues = element.inputValues
            val (newState, outputEvent, newValues) =
                go(currentState, inputEvent, inputValues, currentValues)

            if (outputEvent == element.outputEvent && newValues == element.outputValues) {
                satisfyingStates[j] = newState
            } else {
                break
            }

            currentState = newState
            currentValues = newValues
        }

        return satisfyingStates.asList()
    }

    /**
     * Verify given [positiveScenario].
     *
     * @return `true` if [positiveScenario] is satisfied.
     */
    fun verify(positiveScenario: PositiveScenario): Boolean {
        val satisfyingStates = eval(positiveScenario)
        return satisfyingStates.last() != null
    }

    /**
     * Verify given [negativeScenario].
     *
     * @return `true` if [negativeScenario] is **not** satisfied.
     */
    fun verify(negativeScenario: NegativeScenario, index: Int? = null): Boolean {
        val satisfyingStates = eval(negativeScenario)

        if (negativeScenario.loopPosition != null) {
            val loop = satisfyingStates[negativeScenario.loopPosition - 1]
            val last = satisfyingStates.last()
            if (loop != null && last != null) {
                if (last == loop) {
                    println("[!] Negative scenario${index?.let { " ($index)" } ?: ""} is satisfied (last==loop)")
                    println(">>> satisfyingStates = ${satisfyingStates.map {
                        it?.id ?: 0
                    }} (size = ${satisfyingStates.size})")
                    println(">>> something = ${negativeScenario.elements.map { it.nodeId }}")
                    println(">>> loopPosition = ${negativeScenario.loopPosition}")
                    println(">>> loop = $loop")
                    println(">>> last = $last")
                    println(">>> negativeScenario = $negativeScenario")
                    return false
                }
            }
        } else if (satisfyingStates.last() != null) {
            log.error("Terminal is satisfied")
            return false
        }

        return true
    }

    /**
     * Verify all positive scenarios in given [scenarioTree].
     *
     * @return `true` if **all** scenarios are satisfied.
     */
    fun verify(scenarioTree: ScenarioTree): Boolean =
        scenarioTree.scenarios.all(this@Automaton::verify)

    /**
     * Verify all negative scenarios in given [negativeScenarioTree].
     *
     * @return `true` if **all** scenarios are **not** satisfied.
     */
    fun verify(negativeScenarioTree: NegativeScenarioTree): Boolean =
        negativeScenarioTree.negativeScenarios.mapIndexed { index, scenario -> verify(scenario, index + 1) }.all { it }

    /**
     * Dump automaton in Graphviz, FBT and SMV formats to the [dir] directory using [name] as the file basename.
     */
    fun dump(dir: File, name: String = "automaton") {
        dir.mkdirs()
        dumpGv(dir.resolve("$name.gv"))
        dumpFbt(dir.resolve("$name.fbt"))
        dumpSmv(dir.resolve("$name.smv"))
    }

    /**
     * Dump automaton in Graphviz format to [file].
     */
    fun dumpGv(file: File) {
        file.printWriter().use {
            it.println(this.toGraphvizString())
        }
        Runtime.getRuntime().exec("dot -Tpdf -O $file")
        Runtime.getRuntime().exec("dot -Tpng -O $file")
    }

    /**
     * Dump automaton in FBT format to [file].
     */
    fun dumpFbt(file: File) {
        file.printWriter().use {
            it.println(this.toFbtString())
        }
    }

    /**
     * Dump automaton in SMV format to [file].
     */
    fun dumpSmv(file: File) {
        file.printWriter().use {
            it.println(this.toSmvString())
        }
    }

    /**
     * Pretty-print automaton.
     */
    fun pprint() {
        log.just(toSimpleString().prependIndent("  "))
    }

    /**
     * Stringify automaton to pretty string.
     */
    fun toSimpleString(): String {
        return sequence {
            for (state in states) {
                if (state.transitions.isNotEmpty()) {
                    yield("┌─${state.toSimpleString()}")
                    for (transition in state.transitions.dropLast(1)) {
                        yield("├──${transition.toSimpleString()}")
                    }
                    yield("└──${state.transitions.last().toSimpleString()}")
                } else {
                    yield("──${state.toSimpleString()}")
                }
            }
        }.joinToString("\n")
    }

    /**
     * Stringify automaton to Graphviz format.
     */
    fun toGraphvizString(): String {
        val fontSettings = """fontname="Source Code Pro,monospace" fontsize="12""""
        val setupBlock = """
            graph [$fontSettings]
            node  [$fontSettings]
            edge  [$fontSettings]
        """.trimIndent()

        val nodesBlock = """
            // States
            { node [margin="0.05,0.01"]
            %s
            }
        """.trimIndent().format(
            states
                .joinToString("\n") { it.toGraphvizString() }
                .prependIndent("  ")
        )

        val transitionsBlock = """
            // Transitions
            %s
        """.trimIndent().format(
            transitions.joinToString("\n") { it.toGraphvizString() }
        )

        val body = "%s\n\n%s\n\n%s".format(
            setupBlock,
            nodesBlock,
            transitionsBlock
        )
        return "digraph {\n${body.prependIndent()}\n}"
    }

    /**
     * Stringify automaton to FBT format.
     */
    fun toFbtString(): String {
        fun r() = "%.3f".format((1.0..1000.0).random())

        return xml("FBType") {
            "Identification" {
                attribute("Standard", "61499-2")
            }
            "VersionInfo" {
                attributes(
                    "Organization" to "nxtControl GmbH",
                    "Version" to "0.0",
                    "Author" to "fbSAT",
                    "Date" to "2011-08-30",
                    "Remarks" to "Template",
                    "Namespace" to "Main",
                    "Name" to "CentralController",
                    "Comment" to "Basic Function Block Type"
                )
            }
            "InterfaceList" {
                "InputVars" {
                    for (inputName in inputNames) {
                        "VarDeclaration" {
                            attributes(
                                "Name" to inputName,
                                "Type" to "BOOL"
                            )
                        }
                    }
                }
                "OutputVars" {
                    for (outputName in outputNames) {
                        "VarDeclaration" {
                            attributes(
                                "Name" to outputName,
                                "Type" to "BOOL"
                            )
                        }
                    }
                }
                "EventInputs" {
                    "Event" {
                        attribute("Name", "INIT")
                    }
                    for (inputEvent in listOf("INIT") + inputEvents) {
                        "Event" {
                            attribute("Name", inputEvent)
                            if (inputEvent != "INIT") {
                                for (inputName in inputNames)
                                    "With" {
                                        attribute("Var", inputName)
                                    }
                            }
                        }
                    }
                }
                "EventOutputs" {
                    for (outputEvent in outputEvents) {
                        "Event" {
                            attribute("Name", outputEvent)
                            if (outputEvent != "INITO") {
                                for (outputName in outputNames)
                                    "With" {
                                        attribute("Var", outputName)
                                    }
                            }
                        }
                    }
                }
            }
            "BasicFB" {
                "ECC" {
                    "ECState" {
                        attributes(
                            "Name" to "START",
                            "x" to r(),
                            "y" to r()
                        )
                    }
                    for (state in states) {
                        "ECState" {
                            attributes(
                                "Name" to state.toFbtString(),
                                "x" to r(),
                                "y" to r()
                            )
                            "ECAction" {
                                attributes(
                                    "Algorithm" to (state.algorithm as BinaryAlgorithm).toFbtString(),
                                    "Output" to state.outputEvent
                                )
                            }
                        }
                    }
                    for (transition in transitions)
                        "ECTransition" {
                            attributes(
                                "x" to r(),
                                "y" to r(),
                                "Source" to transition.source.toFbtString(),
                                "Destination" to transition.destination.toFbtString(),
                                "Condition" to transition.toFbtString()
                            )
                        }
                }
                for (algorithm in states.map { it.algorithm as BinaryAlgorithm }.toSet()) {
                    "Algorithm" {
                        attribute("Name", algorithm.toFbtString())
                        "ST" {
                            attribute("Text", algorithm.toST(outputNames))
                        }
                    }
                }
            }
        }.toString(PrintOptions(pretty = true, singleLineTextElements = true, useSelfClosingTags = false))
    }

    /**
     * Stringify automaton to SMV format.
     */
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
            // if (default != null)
            s += "    TRUE : $default;\n"
            return s + "esac"
        }

        val declarations: MutableMap<String, Pair<String, String>> = mutableMapOf() // {name: (init, next)}

        // State declarations
        val stateNextCases = transitions
            .map { it.toSmvString() to it.destination.toSmvString() }
        declarations["_state"] = initialState.toSmvString() to buildCase(stateNextCases, default = "_state")

        // Output events declarations
        for (outputEvent in outputEvents.filter { it != "INITO" }) {
            val outputEventNextCases = states
                .flatMap { it.transitions }
                .filter { it.destination.outputEvent == outputEvent }
                .map { it.toSmvString() to "TRUE" }

            declarations[outputEvent] = "FALSE" to buildCase(outputEventNextCases, default = "FALSE")
        }

        // Output variables declarations
        for (z in 1..outputNames.size) {
            val outputName = outputNames[z - 1]
            val outputVariableNextCases = transitions
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

        val assignBlock = declarations
            .map { (name, declaration) ->
                """
                    init($name) := %s;
                    next($name) := %s;
                """.trimIndent().format(declaration.first, declaration.second)
            }
            .joinToString("\n\n")

        return """
            MODULE $module
            VAR
            %s
            ASSIGN
            %s
        """.trimIndent().format(
            varBlock.prependIndent(),
            assignBlock.prependIndent()
        )
    }

    override fun toString(): String {
        return "Automaton(numberOfStates=$numberOfStates, numberOfTransitions=$numberOfTransitions, totalGuardsSize=$totalGuardsSize, inputEvents=$inputEvents, outputEvents=$outputEvents, inputNames=$inputNames, outputNames=$outputNames)"
    }

    // Allow companion object extensions
    companion object
}
