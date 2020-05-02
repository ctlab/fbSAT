package ru.ifmo.fbsat.core.automaton

import com.github.lipen.lazycache.LazyCache
import ru.ifmo.fbsat.core.automaton.ECC.Action.Companion.eval
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.scenario2.InputAction2
import ru.ifmo.fbsat.core.scenario2.OutputAction2
import ru.ifmo.fbsat.core.scenario2.positive.PositiveScenario2
import ru.ifmo.fbsat.core.scenario2.positive.ScenarioTree2
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.withIndex
import java.io.File

class ECC(
    val inputEvents: List<InputEvent>,
    val outputEvents: List<OutputEvent>,
    val inputNames: List<String>,
    val outputNames: List<String>,
    val initialOutputValues: OutputValues
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
    val transitions: Collection<Transition> by lazyCache {
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
        scenarioTree.outputNames,
        Globals.INITIAL_OUTPUT_VALUES
    )

    constructor(scenarioTree: ScenarioTree2) : this(
        scenarioTree.inputEvents,
        scenarioTree.outputEvents,
        scenarioTree.inputNames,
        scenarioTree.outputNames,
        scenarioTree.initialOutputValues
    )

    fun getState(id: Int): State {
        return _states[id]!!
    }

    fun addState(id: Int, actions: List<Action>) {
        require(id !in _states) { "Automaton already has state '$id'" }
        _states[id] = State(id, actions)
    }

    fun addTransition(sourceId: Int, destinationId: Int, inputEvent: InputEvent, guard: Guard) {
        val source = getState(sourceId)
        val destination = getState(destinationId)
        source.addTransition(destination, inputEvent, guard)
    }

    // TODO: move outside of `ECC`
    // TODO: rename to avoid clashing with OutputAction ._.
    // ECA
    class Action(
        val outputEvent: OutputEvent,
        val algorithm: Algorithm
    ) {
        fun eval(outputValues: OutputValues): OutputAction2 {
            return OutputAction2(
                outputEvent,
                algorithm.eval(outputValues)
            )
        }

        override fun toString(): String {
            return "$outputEvent/$algorithm"
        }

        companion object {
            // TODO: maybe move to State::eval
            fun List<Action>.eval(outputValues: OutputValues): List<OutputAction2> {
                var values = outputValues
                return map { action -> action.eval(values).also { values = it.values } }
            }
        }
    }

    inner class State(
        val id: Int,
        val actions: List<Action>
    ) {
        private val _transitions: MutableList<Transition> = mutableListOf()
        val transitions: List<Transition> = _transitions

        fun addTransition(destination: State, inputEvent: InputEvent, guard: Guard) {
            _transitions.add(Transition(this, destination, inputEvent, guard))
            // ==========
            for ((i, transition) in transitions.withIndex(start = 1))
                check(transition.k == i)
            // ==========
        }

        fun getFirstFiringTransition(inputAction: InputAction2): Transition? =
            transitions.firstOrNull { it.eval(inputAction) }

        // TODO: inline
        fun eval(outputValues: OutputValues): List<OutputAction2> =
            actions.eval(outputValues)

        fun toSimpleString(): String {
            return "$id/$actions"
        }

        fun toGraphvizString(): String {
            TODO()
        }

        fun toFbtString(): String {
            return "s$id"
        }

        fun toSmvString(): String {
            return "s$id"
        }

        override fun toString(): String {
            return "State(id=$id, transitions=${transitions.map { it.destination.id }}, actions=$actions)"
        }
    }

    inner class Transition(
        val source: State,
        val destination: State,
        val inputEvent: InputEvent,
        val guard: Guard
    ) {
        val k: Int = source.transitions.size + 1

        fun eval(inputAction: InputAction2): Boolean {
            return (inputAction.event == null || inputAction.event == inputEvent) && guard.eval(inputAction.values)
        }

        fun toSimpleString(): String {
            return "${source.id} to ${destination.id} on ${inputEvent.name} if ${guard.toSimpleString()}"
        }

        fun toGraphvizString(): String {
            return "$k:${inputEvent.name}/${guard.toGraphvizString()}"
        }

        fun toFbtString(): String {
            return "${inputEvent.name}&(${guard.toFbtString()})"
        }

        fun toSmvString(): String {
            return "_state=${source.toSmvString()} & ${inputEvent.name} & (${guard.toSmvString()})"
        }

        override fun toString(): String {
            return "Transition(k=$k, source=${source.id}, destination=${destination.id}, inputEvent=${inputEvent.name}, guard=$guard)"
        }
    }

    fun eval(stateId: Int, inputAction: InputAction2, outputValues: OutputValues): List<OutputAction2> {
        return getState(stateId).getFirstFiringTransition(inputAction)?.destination?.actions?.eval(outputValues)
            ?: emptyList()
    }

    inner class EvalResult(
        val inputAction: InputAction2,
        val outputActions: List<OutputAction2>,
        val state: State // final state after processing inputAction
    )

    fun eval(
        inputActions: Sequence<InputAction2>
    ): Sequence<EvalResult> {
        var currentState = initialState
        var currentValues = initialOutputValues
        return inputActions.map { inputAction ->
            val transition = currentState.getFirstFiringTransition(inputAction)
            if (transition == null) {
                EvalResult(
                    inputAction = inputAction,
                    outputActions = emptyList(),
                    state = currentState
                )
            } else {
                val destination = transition.destination
                val outputActions = destination.eval(currentValues)
                currentState = destination
                currentValues = outputActions.last().values
                EvalResult(
                    inputAction = inputAction,
                    outputActions = outputActions,
                    state = destination
                )
            }
        }
    }

    /**
     * Verify given [positiveScenario].
     *
     * @return `true` if [positiveScenario] is satisfied.
     */
    fun verify(positiveScenario: PositiveScenario2): Boolean {
        val elements = positiveScenario.elements.asSequence()
        val results = eval(elements.map { it.inputAction })
        for ((i, xxx) in elements.zip(results).withIndex(start = 1)) {
            val (element, result) = xxx
            if (element.outputActions.size != result.outputActions.size) {
                log.error("Inconsistency in the number of output actions for scenario element #$i")
                log.error("element.outputActions (${element.outputActions.size}) = ${element.outputActions}")
                log.error("result.outputActions (${result.outputActions.size}) = ${result.outputActions}")
                return false
            }
            for ((elementOutputAction, resultOutputAction) in element.outputActions.zip(result.outputActions)) {
                if (elementOutputAction != resultOutputAction) {
                    log.error("Inconsistency in output action for scenario element #$i")
                    log.error("elementOutputAction = $elementOutputAction")
                    log.error("resultOutputAction = $resultOutputAction")
                    log.error("element.outputActions (${element.outputActions.size}) = ${element.outputActions}")
                    log.error("result.outputActions (${result.outputActions.size}) = ${result.outputActions}")
                    return false
                }
            }
        }
        return true
    }

    /**
     * Verify all positive scenarios in given [scenarioTree].
     *
     * @return `true` if **all** scenarios are satisfied.
     */
    fun verify(scenarioTree: ScenarioTree2): Boolean =
        scenarioTree.scenarios.all { verify(it) }

    /**
     * Dump automaton in Graphviz, FBT and SMV formats to the [dir] directory using [name] as the file basename.
     */
    fun dump(dir: File, name: String = "automaton") {
        log.warn("ECC dumping is not implemented yet")
        // dir.mkdirs()
        // dumpGv(dir.resolve("$name.gv"))
        // dumpFbt(dir.resolve("$name.fbt"), name)
        // dumpSmv(dir.resolve("$name.smv"))
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
    fun dumpSmv(file: File) {
        file.printWriter().use {
            it.println(toSmvString())
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
        TODO()
    }

    /**
     * Stringify automaton to FBT format.
     */
    fun toFbtString(name: String? = null): String {
        TODO()
    }

    /**
     * Stringify automaton to SMV format.
     */
    fun toSmvString(): String {
        TODO()
    }

    override fun toString(): String {
        return "Automaton(numberOfStates=$numberOfStates, numberOfTransitions=$numberOfTransitions, totalGuardsSize=$totalGuardsSize, inputEvents=$inputEvents, outputEvents=$outputEvents, inputNames=$inputNames, outputNames=$outputNames)"
    }
}

fun ECC.endow(
    C: Int,
    K: Int,
    stateActions: (c: Int) -> List<ECC.Action>,
    transitionDestination: (c: Int, k: Int) -> Int,
    transitionInputEvent: (c: Int, k: Int) -> InputEvent,
    transitionGuard: (c: Int, k: Int) -> Guard
): ECC = apply {
    for (c in 1..C)
        addState(id = c, actions = stateActions(c))

    for (c in 1..C)
        for (k in 1..K) {
            val d = transitionDestination(c, k)
            if (d != 0)
                addTransition(
                    sourceId = c,
                    destinationId = d,
                    inputEvent = transitionInputEvent(c, k),
                    guard = transitionGuard(c, k)
                )
        }
}

@Suppress("LocalVariableName")
fun main() {
    val REQ = InputEvent("REQ")
    val CNF = OutputEvent("CNF")

    val inputNames = listOf("x")
    val outputNames = listOf("z")
    val inputEvents = listOf(REQ)
    val outputEvents = listOf(CNF)
    val initialOutputValues = OutputValues.zeros(outputNames.size)
    Globals.INITIAL_OUTPUT_VALUES = initialOutputValues

    val s = "in=REQ[0]; out=CNF[1]; in=REQ[1]; in=REQ[0]; out=CNF[0]; out=CNF[1];"
    log.info("Trace: $s")
    val scenario = PositiveScenario2.fromString(s)
    log.info("Scenario:")
    for (element in scenario.elements) {
        log.info("    $element")
    }

    val scenarios = listOf(scenario)
    val scenarioTree = ScenarioTree2.fromScenarios(scenarios, inputNames, outputNames, inputEvents, outputEvents)
    log.info("Number of scenarios: ${scenarioTree.scenarios.size}")
    log.info("Number of elements: ${scenarioTree.scenarios.sumBy { it.elements.size }}")
    log.info("Scenario tree size: ${scenarioTree.size}")

    val ecc = ECC(scenarioTree)
    ecc.addState(
        id = 1,
        actions = emptyList()
    )
    ecc.addState(
        id = 2,
        actions = listOf(ECC.Action(CNF, BinaryAlgorithm(listOf(true), listOf(true))))
    )
    ecc.addState(
        id = 3,
        actions = listOf(
            ECC.Action(CNF, BinaryAlgorithm(listOf(true), listOf(false))),
            ECC.Action(CNF, BinaryAlgorithm(listOf(true), listOf(false)))
        )
    )
    ecc.addTransition(
        sourceId = 1,
        destinationId = 2,
        inputEvent = REQ,
        guard = TruthTableGuard(
            mapOf(
                InputValues(listOf(false)) to true,
                InputValues(listOf(true)) to false
            )
        )
    )
    ecc.addTransition(
        sourceId = 2,
        destinationId = 3,
        inputEvent = REQ,
        guard = TruthTableGuard(
            mapOf(
                InputValues(listOf(false)) to true,
                InputValues(listOf(true)) to false
            )
        )
    )

    log.info("ECC:")
    ecc.pprint()

    log.info("Verifying...")
    ecc.verify(scenarioTree)

    log.br()
    log.success("All done.")
}
