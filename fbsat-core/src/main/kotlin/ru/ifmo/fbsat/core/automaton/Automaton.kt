@file:Suppress("LocalVariableName", "MemberVisibilityCanBePrivate")

package ru.ifmo.fbsat.core.automaton

import com.github.lipen.lazycache.LazyCache
import org.redundent.kotlin.xml.PrintOptions
import com.soywiz.klock.DateTime
import org.redundent.kotlin.xml.xml
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.Scenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.utils.graph
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.mutableListOfNulls
import ru.ifmo.fbsat.core.utils.random
import java.io.File

class Automaton(
    val inputEvents: List<InputEvent>,
    val outputEvents: List<OutputEvent>,
    val inputNames: List<String>,
    val outputNames: List<String>
) {
    private val lazyCache = LazyCache()
    private val _states: MutableMap<Int, State> = mutableMapOf()

    /**
     * Automaton states.
     */
    val states: Collection<State> by lazyCache {
        _states.values
    }
    /**
     * Automaton transitions.
     */
    val transitions: Collection<State.Transition> by lazyCache {
        states.flatMap { it.transitions }
    }
    /**
     * Initial automaton state.
     */
    val initialState: State by lazyCache {
        getState(1)
    }
    /**
     * Number of automaton states **C**.
     */
    val numberOfStates: Int by lazyCache {
        states.size
    }
    /**
     * Maximum number of outgoing transitions **K**.
     */
    val maxOutgoingTransitions: Int by lazyCache {
        states.map { it.transitions.size }.max() ?: 0
    }
    /**
     * Maximal guard size **P**.
     */
    val maxGuardSize: Int by lazyCache {
        transitions.map { it.guard.size }.max()!!
    }
    /**
     * Number of automaton transitions **T**.
     */
    val numberOfTransitions: Int by lazyCache {
        transitions.size
    }
    /**
     * Total guards size **N**.
     */
    val totalGuardsSize: Int by lazyCache {
        transitions.sumBy { it.guard.size }
    }

    constructor(scenarioTree: ScenarioTree) : this(
        scenarioTree.inputEvents,
        scenarioTree.outputEvents,
        scenarioTree.inputNames,
        scenarioTree.outputNames
    )

    fun getState(id: Int): State {
        return _states[id]!!
    }

    fun addState(id: Int, outputEvent: OutputEvent?, algorithm: Algorithm) {
        require(id !in _states) { "Automaton already has state '$id'" }
        _states[id] = State(id, outputEvent, algorithm)
        lazyCache.invalidate()
    }

    fun addTransition(sourceId: Int, destinationId: Int, inputEvent: InputEvent, guard: Guard) {
        val source = getState(sourceId)
        val destination = getState(destinationId)
        source.addTransition(destination, inputEvent, guard)
    }

    inner class State(
        val id: Int,
        val outputEvent: OutputEvent?,
        val algorithm: Algorithm
    ) {
        private val _transitions: MutableList<Transition> = mutableListOf()
        val transitions: List<Transition> = _transitions

        fun addTransition(destination: State, inputEvent: InputEvent, guard: Guard) {
            _transitions.add(Transition(destination, inputEvent, guard))
            this@Automaton.lazyCache.invalidate()
        }

        inner class Transition(
            val destination: State,
            val inputEvent: InputEvent?,
            val guard: Guard
        ) {
            val source: State = this@State
            val k: Int = this@State.transitions.size + 1 // Note: 1-based

            fun eval(inputAction: InputAction): Boolean {
                return inputAction.event == inputEvent && guard.eval(inputAction.values)
            }

            fun toSimpleString(): String {
                return "${source.id} to ${destination.id} on ${inputEvent?.name ?: 'ε'} if ${guard.toSimpleString()}"
            }

            fun toGraphvizString(): String {
                return "$k:${inputEvent?.name ?: 'ε'}/${guard.toGraphvizString()}"
            }

            fun toFbtString(): String {
                return "${inputEvent?.name ?: 'ε'}&${guard.toFbtString()}"
            }

            fun toSmvString(): String {
                return if (inputEvent != null)
                    "_state=${source.toSmvString()} & ${inputEvent.name} & (${guard.toSmvString()})"
                else
                    "_state=${source.toSmvString()} & (${guard.toSmvString()})"
            }

            override fun toString(): String {
                return "Transition(k=$k, source=${source.id}, destination=${destination.id}, $inputEvent=${inputEvent?.name
                    ?: 'ε'}, guard=$guard)"
            }
        }

        fun eval(currentValues: OutputValues): OutputAction {
            return OutputAction(outputEvent, algorithm.eval(currentValues))
        }

        fun eval(inputAction: InputAction, currentValues: OutputValues): EvalResult {
            for (transition in transitions) {
                if (transition.eval(inputAction)) {
                    val destination = transition.destination
                    val outputAction = destination.eval(currentValues)
                    return EvalResult(destination, outputAction)
                }
            }
            return EvalResult(this, OutputAction(null, currentValues))
        }

        fun toSimpleString(): String {
            return "$id/${outputEvent?.name ?: 'ε'}(${algorithm.toSimpleString()})"
        }

        fun toGraphvizString(): String {
            // Note: returns html-like label

            algorithm as BinaryAlgorithm
            val Z = outputNames.size

            val vs = (1..Z).mapNotNull { z ->
                val name = outputNames[z - 1]
                val d0 = algorithm.algorithm0[z - 1]
                val d1 = algorithm.algorithm1[z - 1]
                when (d0 to d1) {
                    true to true -> """<TR><TD align="left">$name &rarr; 1</TD></TR>"""
                    false to false -> """<TR><TD align="left">$name &rarr; 0</TD></TR>"""
                    true to false -> """<TR><TD align="left">$name flip</TD></TR>"""
                    false to true -> null
                    else -> error("Bad combination of algorithms ${d0 to d1}")
                }
            }.joinToString("\n")

            val tableBody = if (vs.isNotBlank()) """
                <TR><TD align="center">$id / ${outputEvent?.name ?: 'ε'}</TD></TR>
                <HR/>
                %s
                """.trimIndent().format(vs)
            else """<TR><TD align="center">$id / ${outputEvent?.name ?: 'ε'}</TD></TR>"""

            val html = """
                        <TABLE style="rounded" cellborder="0" cellspacing="0">
                        %s
                        </TABLE>
                    """.trimIndent().format(tableBody.prependIndent())

            return "<\n${html.prependIndent()}>"
        }

        fun toFbtString(): String {
            return "s$id"
        }

        fun toSmvString(): String {
            return "s$id"
        }

        override fun toString(): String {
            return "State(id=$id, outputEvent=${outputEvent?.name
                ?: 'ε'}, algorithm=$algorithm, transitions=${transitions.map { it.destination.id }})"
        }
    }

    data class EvalResult(val destination: State, val outputAction: OutputAction)

    fun eval(
        inputActions: Iterable<InputAction>,
        startState: State = initialState,
        startValues: OutputValues = OutputValues.zeros(outputNames.size)
    ): List<EvalResult> {
        var currentState = startState
        var currentValues = startValues
        return inputActions.map { inputAction ->
            currentState.eval(inputAction, currentValues).also {
                currentState = it.destination
                currentValues = it.outputAction.values
            }
        }
    }

    fun getMapping(scenario: Scenario): List<State?> {
        val mapping: MutableList<State?> = mutableListOfNulls(scenario.elements.size)
        val results = eval(scenario.elements.map { it.inputAction })
        for (i in results.indices) {
            val result = results[i]
            val element = scenario.elements[i]
            if (element.outputAction == result.outputAction) {
                mapping[i] = result.destination
            } else {
                break
            }
        }
        return mapping
    }

    /**
     * Verify given [positiveScenario].
     *
     * @return `true` if [positiveScenario] is satisfied.
     */
    fun verify(positiveScenario: PositiveScenario): Boolean {
        val elements = positiveScenario.elements
        val results = eval(elements.map { it.inputAction })
        for ((i, xxx) in elements.zip(results).withIndex()) {
            val (element, result) = xxx
            if (result.outputAction != element.outputAction) {
                // Contradiction found => positive scenario is not satisfied => verify failure
                println("Contradiction found => positive scenario is not satisfied")
                println("xxx = $xxx")
                println("i+1 = ${i + 1}")
                println("result.outputAction != element.outputAction")
                println("${result.outputAction} != ${element.outputAction}")
                return false
            }
        }
        return true
    }

    /**
     * Verify given [negativeScenario].
     *
     * @return `true` if [negativeScenario] is **not** satisfied.
     */
    fun verify(negativeScenario: NegativeScenario): Boolean {
        val elements = negativeScenario.elements
        val results = eval(elements.map { it.inputAction })

        for ((element, result) in elements.zip(results)) {
            if (result.outputAction != element.outputAction) {
                // Contradiction found => negative scenario is not satisfied => verify ok
                return true
            }
        }

        if (negativeScenario.loopPosition != null) {
            val loop = results.elementAt(negativeScenario.loopPosition - 1)
            val last = results.last()
            @Suppress("LiftReturnOrAssignment")
            if (last.destination == loop.destination) {
                log.error("Negative scenario is satisfied (last==loop)")
                // println(">>> satisfyingStates = ${results.map {
                //     it.destination.id.toString().padStart(2)
                // }} (size = ${results.size})")
                // println(">>>        something = ${negativeScenario.elements.map {
                //     it.nodeId.toString().padStart(2)
                // }}")
                // println(">>> loopPosition = ${negativeScenario.loopPosition}")
                // println(">>> loop = $loop")
                // println(">>> last = $last")
                // println(">>> negativeScenario = $negativeScenario")
                // Last and loop-back elements are satisfied by the same state
                return false
            } else {
                // Last and loop-back elements are satisfied by different states
                return true
            }
        } else {
            log.error("Terminal is satisfied")
            return false
        }
    }

    /**
     * Verify all positive scenarios in given [scenarioTree].
     *
     * @return `true` if **all** scenarios are satisfied.
     */
    fun verify(scenarioTree: ScenarioTree): Boolean =
        scenarioTree.scenarios.all(::verify)

    /**
     * Verify all negative scenarios in given [negativeScenarioTree].
     *
     * @return `true` if **all** scenarios are **not** satisfied.
     */
    fun verify(negativeScenarioTree: NegativeScenarioTree): Boolean =
        negativeScenarioTree.negativeScenarios.all(::verify)

    /**
     * Dump automaton in Graphviz, FBT and SMV formats to the [dir] directory using [name] as the file basename.
     */
    fun dump(dir: File, name: String = "automaton") {
        dir.mkdirs()
        dumpGv(dir.resolve("$name.gv"))
        dumpFbt(dir.resolve("$name.fbt"), name)
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
        // Runtime.getRuntime().exec("dot -Tpng -O $file")
    }

    /**
     * Dump automaton in FBT format to [file].
     */
    fun dumpFbt(file: File, name: String? = null) {
        file.printWriter().use {
            it.println(this.toFbtString(name))
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
        return graph("Automaton") {
            setup(
                "fontname" to "Source Code Pro,monospace",
                "fontsize" to "12"
            )
            nodeSetup(
                "margin" to "0.05,0.01",
                "shape" to "plaintext"
            )

            for (state in states)
                node(
                    state.id,
                    "label" to state.toGraphvizString()
                )

            for (transition in transitions)
                edge(
                    transition.source.id,
                    transition.destination.id,
                    "label" to transition.toGraphvizString()
                )
        }.toGraphvisString()
    }

    /**
     * Stringify automaton to FBT format.
     */
    fun toFbtString(name: String? = null): String {
        fun r() = "%.3f".format((1.0..1000.0).random())
        val Z = outputNames.size

        return xml("FBType") {
            if (name != null) {
                attribute("Name", name)
            }
            "Identification"("Standard" to "61499-2")
            "VersionInfo"(
                "Organization" to "nxtControl GmbH",
                "Version" to "0.0",
                "Author" to "fbSAT",
                "Date" to DateTime.nowLocal().format("yyyy-MM-dd")
            )
            "InterfaceList" {
                "EventInputs" {
                    // Note: INIT input event has no associated variables
                    "Event"("Name" to "INIT")
                    for (inputEvent in inputEvents) {
                        "Event"("Name" to inputEvent.name) {
                            for (inputName in inputNames)
                                "With"("Var" to inputName)
                        }
                    }
                }
                "EventOutputs" {
                    // Note: INITO output event has the same associated variables as all output events
                    for (outputEvent in outputEvents + OutputEvent("INITO")) {
                        "Event"("Name" to outputEvent.name) {
                            for (outputName in outputNames)
                                "With"("Var" to outputName)
                        }
                    }
                }
                "InputVars" {
                    for (inputName in inputNames) {
                        "VarDeclaration"(
                            "Name" to inputName,
                            "Type" to "BOOL"
                        )
                    }
                }
                "OutputVars" {
                    for (outputName in outputNames) {
                        "VarDeclaration"(
                            "Name" to outputName,
                            "Type" to "BOOL"
                        )
                    }
                }
            }
            "BasicFB" {
                "ECC" {
                    "ECState"(
                        "Name" to "START", // START state
                        "x" to r(), "y" to r()
                    )
                    "ECState"(
                        "Name" to "INIT", // INIT state
                        "x" to r(), "y" to r()
                    ) {
                        "ECAction"(
                            "Algorithm" to "INIT", // INIT algorithm
                            "Output" to "INITO" // INITO output event
                        )
                    }
                    for (state in states) {
                        "ECState"(
                            "Name" to state.toFbtString(),
                            "x" to r(), "y" to r()
                        ) {
                            state.algorithm as BinaryAlgorithm
                            "ECAction"(
                                "Algorithm" to "${state.id}_${state.algorithm.toFbtString()}"
                            ) {
                                if (state.outputEvent != null)
                                    attribute("Output", state.outputEvent.name)
                            }
                        }
                    }
                    "ECTransition"(
                        "Source" to "START", // START state
                        "Destination" to "INIT", // INIT state
                        "Condition" to "INIT", // INIT input event
                        "x" to r(), "y" to r()
                    )
                    "ECTransition"(
                        "Source" to "INIT", // INIT state
                        "Destination" to initialState.toFbtString(),
                        "Condition" to "1",
                        "x" to r(), "y" to r()
                    )
                    for (transition in transitions) {
                        "ECTransition"(
                            "Source" to transition.source.toFbtString(),
                            "Destination" to transition.destination.toFbtString(),
                            "Condition" to transition.toFbtString(),
                            "x" to r(), "y" to r()
                        )
                    }
                }
                "Algorithm"(
                    "Name" to "INIT" // INIT algorithm
                ) {
                    // Note: INIT algorithm zeroes out all variables
                    "ST"(
                        "Text" to BinaryAlgorithm(
                            BooleanArray(Z) { false },
                            BooleanArray(Z) { false }
                        ).toST(outputNames)
                    )
                }
                for (state in states) {
                    if (state.outputEvent == null)
                        continue

                    val algorithm = state.algorithm as BinaryAlgorithm
                    "Algorithm"(
                        "Name" to "${state.id}_${algorithm.toFbtString()}"
                    ) {
                        "ST"(
                            "Text" to algorithm.toST(outputNames)
                        )
                    }
                }
            }
        }.toString(PrintOptions(pretty = true, singleLineTextElements = true, useSelfClosingTags = false))
    }

    /**
     * Stringify automaton to SMV format.
     */
    fun toSmvString(): String {
        val module = "CONTROL(${inputEvents.joinToString(",") { it.name }}, ${inputNames.joinToString(",")})"

        val definitions: MutableMap<String, String> = mutableMapOf(
            "_state" to "{${states.joinToString(", ") { it.toSmvString() }}}"
        )
        for (outputEvent in outputEvents)
            definitions[outputEvent.name] = "boolean"
        for (outputName in outputNames)
            definitions[outputName] = "boolean"

        fun buildCase(cases: List<Pair<String, String>>, default: String): String {
            var s = "case\n"
            for ((lhs, rhs) in cases)
                s += "    $lhs : $rhs;\n"
            s += "    TRUE : $default;\n"
            return s + "esac"
        }

        val declarations: MutableMap<String, Pair<String, String>> = mutableMapOf() // {name: (init, next)}

        // State declarations
        val stateNextCases = transitions
            .map { it.toSmvString() to it.destination.toSmvString() }
        declarations["_state"] = initialState.toSmvString() to buildCase(stateNextCases, default = "_state")

        // Output events declarations
        for (outputEvent in outputEvents) {
            val outputEventNextCases = states
                .flatMap { it.transitions }
                .filter { it.destination.outputEvent == outputEvent }
                .map { it.toSmvString() to "TRUE" }
            declarations[outputEvent.name] = "FALSE" to buildCase(outputEventNextCases, default = "FALSE")
        }

        // Output variables declarations
        for (z in 1..outputNames.size) {
            val outputName = outputNames[z - 1]
            val outputVariableNextCases = transitions
                .flatMap {
                    val condition = it.toSmvString()
                    val algo = it.destination.algorithm as BinaryAlgorithm
                    listOf(
                        "$condition & !$outputName" to algo.algorithm0[z - 1].toString().toUpperCase(),
                        "$condition & $outputName" to algo.algorithm1[z - 1].toString().toUpperCase()
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
