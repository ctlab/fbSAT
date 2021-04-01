@file:Suppress("LocalVariableName", "MemberVisibilityCanBePrivate")

package ru.ifmo.fbsat.core.automaton

import com.github.lipen.lazycache.LazyCache
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.core.Context
import com.github.lipen.satlib.core.Model
import com.soywiz.klock.DateTime
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.serializer
import org.redundent.kotlin.xml.xml
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.Scenario
import ru.ifmo.fbsat.core.scenario.inputActionsSeq
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenario
import ru.ifmo.fbsat.core.scenario.negative.OldNegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.convertBoolVarArray
import ru.ifmo.fbsat.core.solver.convertDomainVarArray
import ru.ifmo.fbsat.core.solver.convertIntVarArray
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.graph
import ru.ifmo.fbsat.core.utils.mutableListOfNulls
import ru.ifmo.fbsat.core.utils.random
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.withIndex
import java.io.File

private val logger = MyLogger {}

@Serializable
data class AutomatonSurrogate(
    val states: List<State>,
    val inputEvents: List<InputEvent>,
    val outputEvents: List<OutputEvent>,
    val inputNames: List<String>,
    val outputNames: List<String>,
) {
    @Serializable
    data class State(
        val id: Int,
        val outputEvent: OutputEvent?,
        val algorithm: Algorithm,
        val transitions: List<Transition>,
    )

    @Serializable
    data class Transition(
        val source: Int,
        val destination: Int,
        val inputEvent: InputEvent?,
        var guard: Guard,
    )
}

object AutomatonSerializer : KSerializer<Automaton> {
    override val descriptor: SerialDescriptor = AutomatonSurrogate.serializer().descriptor

    override fun serialize(encoder: Encoder, value: Automaton) {
        val states: List<AutomatonSurrogate.State> = value.states.map { s ->
            AutomatonSurrogate.State(
                id = s.id,
                outputEvent = s.outputEvent,
                algorithm = s.algorithm,
                transitions = s.transitions.map { t ->
                    AutomatonSurrogate.Transition(
                        source = t.source.id,
                        destination = t.destination.id,
                        inputEvent = t.inputEvent,
                        guard = t.guard
                    )
                }
            )
        }
        val surrogate = AutomatonSurrogate(
            states, value.inputEvents, value.outputEvents, value.inputNames, value.outputNames
        )
        encoder.encodeSerializableValue(serializer(), surrogate)
    }

    override fun deserialize(decoder: Decoder): Automaton {
        val surrogate: AutomatonSurrogate = decoder.decodeSerializableValue(serializer())
        val automaton = Automaton(
            inputEvents = surrogate.inputEvents,
            outputEvents = surrogate.outputEvents,
            inputNames = surrogate.inputNames,
            outputNames = surrogate.outputNames
        )
        for (state in surrogate.states) {
            automaton.addState(
                id = state.id,
                outputEvent = state.outputEvent,
                algorithm = state.algorithm
            )
        }
        for (state in surrogate.states) {
            for (transition in state.transitions) {
                automaton.addTransition(
                    sourceId = transition.source,
                    destinationId = transition.destination,
                    inputEvent = transition.inputEvent,
                    guard = transition.guard
                )
            }
        }
        return automaton
    }
}

@Serializable(with = AutomatonSerializer::class)
class Automaton(
    val inputEvents: List<InputEvent>,
    val outputEvents: List<OutputEvent>,
    val inputNames: List<String>,
    val outputNames: List<String>,
) {
    private val lazyCache = LazyCache()
    private val _states: MutableMap<Int, State> = mutableMapOf()

    /** Automaton states. */
    val states: Collection<State> by lazyCache {
        _states.values
    }

    /** Automaton transitions. */
    val transitions: Collection<Transition> by lazyCache {
        states.flatMap { it.transitions }
    }

    /** Initial automaton state. */
    val initialState: State by lazyCache {
        getState(1)
    }

    /** Number of automaton states **C**. */
    val numberOfStates: Int by lazyCache {
        states.size
    }

    /**
     * Number of reachable automaton states.
     */
    val numberOfReachableStates: Int by lazyCache {
        (transitions.map { it.destination } + initialState).toSet().size
    }

    /** Maximum number of outgoing transitions **K**. */
    val maxOutgoingTransitions: Int by lazyCache {
        states.map { it.transitions.size }.maxOrNull() ?: 0
    }

    /** Maximal guard size **P**. */
    val maxGuardSize: Int by lazyCache {
        transitions.map { it.guard.size }.maxOrNull() ?: 0
    }

    /** Number of automaton transitions **T**. */
    val numberOfTransitions: Int by lazyCache {
        transitions.size
    }

    /** Total guards size **N**. */
    val totalGuardsSize: Int by lazyCache {
        transitions.sumBy { it.guard.size }
    }

    constructor(scenarioTree: PositiveScenarioTree) : this(
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

    fun addTransition(sourceId: Int, destinationId: Int, inputEvent: InputEvent?, guard: Guard) {
        val source = getState(sourceId)
        val destination = getState(destinationId)
        source.addTransition(destination, inputEvent, guard)
    }

    inner class State(
        val id: Int,
        val outputEvent: OutputEvent?,
        val algorithm: Algorithm,
    ) {
        private val _transitions: MutableList<Transition> = mutableListOf()
        val transitions: List<Transition> = _transitions

        fun addTransition(destination: State, inputEvent: InputEvent?, guard: Guard) {
            _transitions.add(Transition(this, destination, inputEvent, guard))
            this@Automaton.lazyCache.invalidate()
        }

        fun eval(currentValues: OutputValues): OutputAction {
            return OutputAction(
                outputEvent,
                algorithm.eval(currentValues)
            )
        }

        fun eval(inputAction: InputAction, currentValues: OutputValues): EvalResult {
            // TODO: check if input event is not null -- if so, we do not need to eval all transitions at all
            for (transition in transitions) {
                if (transition.eval(inputAction)) {
                    val destination = transition.destination
                    val outputAction = destination.eval(currentValues)
                    return EvalResult(inputAction, destination, outputAction)
                }
            }
            return EvalResult(inputAction, this, OutputAction(null, currentValues))
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
                |<TR><TD align="center">$id / ${outputEvent?.name ?: 'ε'}</TD></TR>
                |<HR/>
                |%s
                |""".trimMargin().format(vs)
            else """<TR><TD align="center">$id / ${outputEvent?.name ?: 'ε'}</TD></TR>"""

            val html = """
                |<TABLE style="rounded" cellborder="0" cellspacing="0">
                |%s
                |</TABLE>
            """.trimMargin().format(tableBody.prependIndent())

            return "<\n${html.prependIndent()}>"
        }

        fun toFbtString(): String {
            return "s$id"
        }

        fun toSmvString(): String {
            return "s$id"
        }

        override fun toString(): String {
            return "State(id=$id, outputEvent=${
                outputEvent?.name
                    ?: 'ε'
            }, algorithm=$algorithm, transitions=${transitions.map { it.destination.id }})"
        }
    }

    inner class Transition(
        val source: State,
        val destination: State,
        val inputEvent: InputEvent?,
        var guard: Guard,
    ) {
        val k: Int = source.transitions.size + 1 // Note: 1-based

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
            return if (inputEvent != null) {
                "_state=${source.toSmvString()} & ${inputEvent.name} & (${guard.toSmvString()})"
            } else {
                "_state=${source.toSmvString()} & (${guard.toSmvString()})"
            }
        }

        override fun toString(): String {
            return "Transition(k=$k, source=${source.id}, destination=${destination.id}, $inputEvent=${
                inputEvent?.name
                    ?: 'ε'
            }, guard=$guard)"
        }
    }

    // TODO: Rewrite all `eval` methods to use EvalState
    data class EvalState(
        val state: Automaton.State,
        val outputValues: OutputValues,
    ) {
        fun eval(inputAction: InputAction): EvalResult {
            return state.eval(inputAction, outputValues)
        }
    }

    data class EvalResult(
        val inputAction: InputAction,
        val destination: State,
        val outputAction: OutputAction,
    ) {
        val newEvalState: EvalState = EvalState(destination, outputAction.values)

        override fun toString(): String {
            return "EvalResult(destination = ${destination.id}, outputAction = $outputAction)"
        }
    }

    @Deprecated("Use evalState.eval directly", ReplaceWith("evalState.eval(inputAction)"))
    fun eval(
        inputAction: InputAction,
        evalState: EvalState,
    ): EvalResult =
        evalState.eval(inputAction)

    @Deprecated("Use eval(InputAction, EvalState)")
    fun eval(
        inputAction: InputAction,
        state: State,
        values: OutputValues,
    ): EvalResult =
        EvalState(state, values).eval(inputAction)

    fun eval(
        inputActions: Sequence<InputAction>,
        startEvalState: EvalState,
    ): Sequence<EvalResult> {
        var currentEvalState = startEvalState
        return inputActions.map { inputAction ->
            currentEvalState.eval(inputAction).also {
                currentEvalState = it.newEvalState
            }
        }
    }

    fun eval(
        inputActions: Sequence<InputAction>,
        startState: State = initialState,
        startValues: OutputValues = Globals.INITIAL_OUTPUT_VALUES,
    ): Sequence<EvalResult> =
        eval(inputActions, EvalState(startState, startValues))

    fun eval(
        inputActions: Iterable<InputAction>,
        startState: State = initialState,
        startValues: OutputValues = Globals.INITIAL_OUTPUT_VALUES,
    ): List<EvalResult> =
        eval(inputActions.asSequence(), startState, startValues).toList()

    /**
     * Evaluate the given [scenario].
     */
    fun eval(scenario: Scenario): Sequence<EvalResult> =
        eval(scenario.inputActionsSeq)

    private fun map_(scenario: Scenario): List<EvalResult?> {
        val mapping: MutableList<EvalResult?> = mutableListOfNulls(scenario.elements.size)
        out@ for ((i, result) in eval(scenario).withIndex()) {
            val element = scenario.elements[i]
            if (result.outputAction != element.outputAction) {
                // log.error("No mapping for ${i + 1}-th element = $element, result = $result")
                break@out
            }
            mapping[i] = result
        }
        return mapping
    }

    /**
     * Map the given [scenario].
     * Nulls represent the absence of a mapping.
     */
    fun map(scenario: Scenario): List<State?> =
        map_(scenario).map { it?.destination }

    /**
     * Verify the given [positiveScenario].
     *
     * @return `true` if [positiveScenario] is satisfied.
     */
    fun verify(positiveScenario: PositiveScenario): Boolean {
        val mapping: List<State?> = map(positiveScenario)
        return mapping.last() != null
    }

    /**
     * Verify the given [negativeScenario].
     *
     * @return `true` if [negativeScenario] is **not** satisfied.
     */
    fun verify(negativeScenario: NegativeScenario): Boolean {
        val mapping: List<State?> = map(negativeScenario)
        if (negativeScenario.loopPosition != null) {
            val loop = mapping[negativeScenario.loopPosition - 1]
            val last = mapping.last()
            return when {
                loop == null || last == null -> {
                    // Either `loop` or `last` elements are unmapped,
                    // which means that the negative scenario is not satisfied.
                    true
                }
                loop.id == last.id -> {
                    // Both `loop` and `last` elements map to the same state,
                    // which means that the negative scenario is satisfied.
                    logger.error("Negative scenario is satisfied (loop==last)")
                    false
                }
                else -> {
                    // `loop` and `last` elements map to different states,
                    // which means that the negative scenario is not satisfied.
                    true
                }
            }
        } else {
            val last = mapping.last()
            return if (last != null) {
                // Satisfaction of the terminal (`last`) element of loop-less negative scenario
                // implies the satisfaction of the negative scenario itself.
                logger.error("Negative scenario is satisfied (terminal)")
                false
            } else {
                // Terminal (`last`) element of loop-less negative scenario is unmapped,
                // which means that the negative scenario is not satisfied.
                true
            }
        }
    }

    /**
     * Verify all positive scenarios in given [scenarioTree].
     *
     * @return `true` if **all** scenarios are satisfied.
     */
    fun verify(scenarioTree: OldPositiveScenarioTree): Boolean =
        scenarioTree.scenarios.all(::verify)

    /**
     * Verify all positive scenarios in given [positiveScenarioTree].
     *
     * @return `true` if **all** scenarios are satisfied.
     */
    fun verify(positiveScenarioTree: PositiveScenarioTree): Boolean =
        positiveScenarioTree.scenarios.all(::verify)

    /**
     * Verify all negative scenarios in given [negativeScenarioTree].
     *
     * @return `true` if **all** scenarios are **not** satisfied.
     */
    fun verify(negativeScenarioTree: OldNegativeScenarioTree): Boolean =
        negativeScenarioTree.negativeScenarios.all(::verify)

    /**
     * Dump automaton in Graphviz, FBT and SMV formats to the [dir] directory using [name] as the file basename.
     */
    fun dump(dir: File, name: String = "automaton") {
        logger.info("Dumping '$name' to <$dir>...")
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
            it.println(toGraphvizString())
        }
        Runtime.getRuntime().exec("dot -Tpdf -O $file")
        // Runtime.getRuntime().exec("dot -Tpng -O $file")
    }

    /**
     * Dump automaton in FBT format to [file].
     */
    fun dumpFbt(file: File, name: String? = null) {
        file.printWriter().use {
            it.println(toFbtString(name))
        }
    }

    /**
     * Dump automaton in SMV format to [file].
     */
    fun dumpSmv(file: File, name: String = "CONTROL") {
        file.printWriter().use {
            it.println(toSmvString(name))
        }
    }

    fun calculateHashCode(): Int {
        val codeOutputEvents =
            states.map {
                if (it.outputEvent != null) outputEvents.indexOf(it.outputEvent) + 1 else 0
            }
        val codeAlgorithms =
            states.map {
                with(it.algorithm as BinaryAlgorithm) {
                    algorithm0.toBinaryString().toInt(2) + algorithm1.toBinaryString().toInt(2)
                }
            }
        val codeTransitionDestination =
            transitions.map {
                it.destination.id
            }
        val codeTransitionEvents =
            transitions.map {
                if (it.inputEvent != null) inputEvents.indexOf(it.inputEvent) + 1 else 0
            }
        val codeTransitionGuards =
            transitions.flatMap {
                it.guard.truthTableString
                    .windowed(8, step = 8, partialWindows = true)
                    .map { s -> s.toInt(2) }
            }
        return (
            codeOutputEvents +
                codeAlgorithms +
                codeTransitionDestination +
                codeTransitionEvents +
                codeTransitionGuards
            )
            .fold(0) { acc, i ->
                (31 * acc + i).rem(1_000_000)
            }
    }

    /**
     * Pretty-print automaton.
     */
    fun pprint() {
        logger.just("Automaton Hash-Code: ${calculateHashCode()}")
        logger.just(toSimpleString().prependIndent("  "))
    }

    // fun toPrettyString(): String {
    //     return "Automaton Hash-Code: ${calculateHashCode()}\n" +
    //         toSimpleString().prependIndent("  ")
    // }

    fun getStats(): String {
        return "" +
            "C = $numberOfStates, " +
            "K = $maxOutgoingTransitions, " +
            "P = $maxGuardSize, " +
            "T = $numberOfTransitions, " +
            "N = $totalGuardsSize"
    }

    fun printStats() {
        logger.info("    " + getStats())
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

        return xml("FBType") {
            if (name != null) {
                attribute("Name", name)
            }
            attribute("Namespace", "Main")
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
                            for (inputName in inputNames) {
                                "With"("Var" to inputName)
                            }
                        }
                    }
                }
                "EventOutputs" {
                    // Note: INITO output event has the same associated variables as all output events
                    for (outputEvent in outputEvents + OutputEvent("INITO")) {
                        "Event"("Name" to outputEvent.name) {
                            for (outputName in outputNames) {
                                "With"("Var" to outputName)
                            }
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
                            "ECAction"("Algorithm" to "s${state.id}_${state.algorithm.toFbtString()}") {
                                if (state.outputEvent != null) {
                                    attribute("Output", state.outputEvent.name)
                                }
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
                "Algorithm"("Name" to "INIT") {
                    val a = Globals.INITIAL_OUTPUT_VALUES.values.toBooleanArray()
                    "ST"(
                        "Text" to BinaryAlgorithm(a, a).toST(outputNames)
                    )
                }
                for (state in states) {
                    if (state.outputEvent == null) continue

                    val algorithm = state.algorithm as BinaryAlgorithm
                    "Algorithm"("Name" to "s${state.id}_${algorithm.toFbtString()}") {
                        "ST"("Text" to algorithm.toST(outputNames))
                    }
                }
            }
        }.toString(Globals.xmlPrintOptions)
    }

    /**
     * Stringify automaton to SMV format.
     */
    fun toSmvString(name: String = "CONTROL"): String {
        val module = "$name(${inputEvents.joinToString(",") { it.name }}, ${inputNames.joinToString(",")})"

        val definitions: MutableMap<String, String> = mutableMapOf()
        if (states.size > 1) {
            definitions["_state"] = "{${states.joinToString(", ") { it.toSmvString() }}}"
        } else {
            // Adhoc: add one more aux state
            definitions["_state"] = "{${states.joinToString(", ") { it.toSmvString() }}, you_should_not_be_here}"
        }
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
}

fun Automaton.endow(
    C: Int,
    K: Int,
    stateUsed: (c: Int) -> Boolean = { _ -> true },
    stateOutputEvent: (c: Int) -> OutputEvent?,
    stateAlgorithm: (c: Int) -> Algorithm,
    transitionDestination: (c: Int, k: Int) -> Int,
    transitionInputEvent: (c: Int, k: Int) -> InputEvent,
    transitionGuard: (c: Int, k: Int) -> Guard,
): Automaton = apply {
    for (c in 1..C)
        if (stateUsed(c))
            addState(
                id = c,
                outputEvent = stateOutputEvent(c),
                algorithm = stateAlgorithm(c)
            )

    for (c in 1..C)
        for (k in 1..K) {
            val d = transitionDestination(c, k)
            if (d != 0) addTransition(
                sourceId = c,
                destinationId = d,
                inputEvent = transitionInputEvent(c, k),
                guard = transitionGuard(c, k)
            )
        }
}

fun buildBasicAutomaton(
    context: Context,
    model: Model,
    useStateUsed: Boolean = false,
): Automaton {
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val C: Int = context["C"]
    val K: Int = context["K"]
    val Z: Int = context["Z"]
    val transitionDestination = context.convertIntVarArray("transitionDestination", model)
    val transitionInputEvent = context.convertIntVarArray("transitionInputEvent", model)
    val transitionTruthTable = context.convertBoolVarArray("transitionTruthTable", model)
    val stateOutputEvent = context.convertIntVarArray("stateOutputEvent", model)
    val stateAlgorithmTop = context.convertBoolVarArray("stateAlgorithmTop", model)
    val stateAlgorithmBot = context.convertBoolVarArray("stateAlgorithmBot", model)

    val stateUsedFunction: (c: Int) -> Boolean =
        if (useStateUsed) {
            val stateUsed = context.convertBoolVarArray("stateUsed", model);
            { c -> stateUsed[c] }
        } else {
            { true }
        }

    return Automaton(scenarioTree).endow(
        C = C, K = K,
        stateUsed = stateUsedFunction,
        stateOutputEvent = { c ->
            stateOutputEvent[c].let { o ->
                if (o == 0) null else scenarioTree.outputEvents[o - 1]
            }
        },
        stateAlgorithm = { c ->
            BinaryAlgorithm(
                algorithm0 = BooleanArray(Z) { z0 -> stateAlgorithmBot[c, z0 + 1] },
                algorithm1 = BooleanArray(Z) { z0 -> stateAlgorithmTop[c, z0 + 1] }
            )
        },
        transitionDestination = { c, k ->
            transitionDestination[c, k]
        },
        transitionInputEvent = { c, k ->
            scenarioTree.inputEvents[transitionInputEvent[c, k] - 1]
        },
        transitionGuard = { c, k ->
            TruthTableGuard(
                truthTable = scenarioTree.uniqueInputs
                    .withIndex(start = 1)
                    .associate { (u, input) ->
                        input to transitionTruthTable[c, k, u]
                    }
                // inputNames = scenarioTree.inputNames,
                // uniqueInputs = scenarioTree.uniqueInputs
            )
        }
    )
}

fun buildExtendedAutomaton(
    context: Context,
    model: Model,
    useStateUsed: Boolean = false,
): Automaton {
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val C: Int = context["C"]
    val K: Int = context["K"]
    val P: Int = context["P"]
    val Z: Int = context["Z"]
    val transitionDestination = context.convertIntVarArray("transitionDestination", model)
    val transitionInputEvent = context.convertIntVarArray("transitionInputEvent", model)
    val stateOutputEvent = context.convertIntVarArray("stateOutputEvent", model)
    val stateAlgorithmTop = context.convertBoolVarArray("stateAlgorithmTop", model)
    val stateAlgorithmBot = context.convertBoolVarArray("stateAlgorithmBot", model)
    val nodeType = context.convertDomainVarArray<NodeType>("nodeType", model)
    val nodeInputVariable = context.convertIntVarArray("nodeInputVariable", model)
    val nodeParent = context.convertIntVarArray("nodeParent", model)
    val nodeChild = context.convertIntVarArray("nodeChild", model)

    val stateUsedFunction: (c: Int) -> Boolean =
        if (useStateUsed) {
            val stateUsed = context.convertBoolVarArray("stateUsed", model);
            { c -> stateUsed[c] }
        } else {
            { true }
        }

    return Automaton(scenarioTree).endow(
        C = C, K = K,
        stateUsed = stateUsedFunction,
        stateOutputEvent = { c ->
            stateOutputEvent[c].let { o ->
                if (o == 0) null
                else scenarioTree.outputEvents[o - 1]
            }
        },
        stateAlgorithm = { c ->
            BinaryAlgorithm(
                algorithm0 = BooleanArray(Z) { z0 -> stateAlgorithmBot[c, z0 + 1] },
                algorithm1 = BooleanArray(Z) { z0 -> stateAlgorithmTop[c, z0 + 1] }
            )
        },
        transitionDestination = { c, k ->
            transitionDestination[c, k]
        },
        transitionInputEvent = { c, k ->
            scenarioTree.inputEvents[transitionInputEvent[c, k] - 1]
        },
        transitionGuard = { c, k ->
            ParseTreeGuard(
                nodeType = MultiArray.new(P) { (p) -> nodeType[c, k, p] },
                terminal = IntMultiArray.new(P) { (p) -> nodeInputVariable[c, k, p] },
                parent = IntMultiArray.new(P) { (p) -> nodeParent[c, k, p] },
                childLeft = IntMultiArray.new(P) { (p) -> nodeChild[c, k, p] },
                childRight = IntMultiArray.new(P) { (p) ->
                    if (nodeType[c, k, p] in setOf(NodeType.AND, NodeType.OR))
                        nodeChild[c, k, p] + 1
                    else
                        0
                },
                inputNames = scenarioTree.inputNames
            )
        }
    )
}
