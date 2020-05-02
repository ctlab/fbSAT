@file:Suppress("MemberVisibilityCanBePrivate")

package ru.ifmo.fbsat.core.scenario2.positive

import com.github.lipen.lazycache.LazyCache
import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.automaton.OutputValues
import ru.ifmo.fbsat.core.scenario2.InputAction2
import ru.ifmo.fbsat.core.scenario2.OutputAction2
import ru.ifmo.fbsat.core.scenario2.ScenarioElement2
import ru.ifmo.fbsat.core.scenario2.ScenarioTreeInterface2
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.toBinaryString
import java.io.File

class ScenarioTree2(
    override val inputEvents: List<InputEvent>,
    override val outputEvents: List<OutputEvent>,
    override val inputNames: List<String>,
    override val outputNames: List<String>,
    override val initialOutputValues: OutputValues = Globals.INITIAL_OUTPUT_VALUES,
    private val isTrie: Boolean = true
) : ScenarioTreeInterface2 {
    private val lazyCache = LazyCache()
    private val _scenarios: MutableList<PositiveScenario2> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    val scenarios: List<PositiveScenario2> = _scenarios
    val nodes: List<Node> = _nodes
    val root: Node? by lazyCache {
        nodes.firstOrNull()
    }
    override val size: Int by lazyCache {
        nodes.size
    }

    // Note: all public lists are zero-based

    override val uniqueInputs: List<InputValues> by lazyCache {
        nodes.asSequence().drop(1).map { it.inputValues }.toSet().toList().sortedBy { it.values.toBinaryString() }
    }
    override val uniqueOutputs: List<OutputValues> by lazyCache {
        nodes.asSequence()
            .drop(1)
            .flatMap { node ->
                node.outputActions.map { outputAction -> outputAction.values }.asSequence()
            }
            .toSet()
            .sortedBy { it.values.toBinaryString() }
    }

    /**
     * List of **active** vertices, i.e. vertices with **non-null** output event.
     * The root is excluded explicitly.
     */
    val activeVertices: List<Int> by lazyCache {
        nodes.asSequence()
            .drop(1) // without root
            .filter { it.isActive() }
            .map { it.id }
            .toList()
    }

    /**
     * List of **passive** vertices, i.e. vertices with **null** (aka empty/epsilon) output event.
     * The root is excluded explicitly.
     */
    val passiveVertices: List<Int> by lazyCache {
        nodes.asSequence()
            .drop(1) // without root
            .filter { it.isPassive() }
            .map { it.id }
            .toList()
    }

    inner class Node(
        val element: ScenarioElement2,
        val parent: Node?
    ) {
        private val _children: MutableList<Node> = mutableListOf()

        val id: Int = this@ScenarioTree2.size + 1 // Note: one-based
        val children: List<Node> = _children

        // val previousActive: Node? = if (parent?.outputEvent != null) parent else parent?.previousActive
        val inputAction: InputAction2 = element.inputAction
        val outputActions: List<OutputAction2> = element.outputActions
        val inputEvent: InputEvent = inputAction.event
        val inputValues: InputValues = inputAction.values
        val finalOutputValues: OutputValues =
            if (outputActions.isNotEmpty())
                outputActions.last().values
            else
                parent?.finalOutputValues ?: this@ScenarioTree2.initialOutputValues

        init {
            if (id > 1) {
                require(element.inputEvent == null || element.inputEvent in this@ScenarioTree2.inputEvents) {
                    "Unexpected input event '${element.inputEvent}'"
                }
                require(element.inputValues.values.size == this@ScenarioTree2.inputNames.size) {
                    "Unexpected number of input values (element: ${element.inputValues.values.size}, tree: ${this@ScenarioTree2.inputNames.size})"
                }
                for (outputAction in element.outputActions) {
                    require(outputAction.event == null || outputAction.event in this@ScenarioTree2.outputEvents) {
                        "Unexpected output event '${outputAction.event}'"
                    }
                    require(outputAction.values.values.size == this@ScenarioTree2.outputNames.size) {
                        "Unexpected number of output values (element: ${outputAction.values.values.size}, tree: ${this@ScenarioTree2.outputNames.size})"
                    }
                }
            }

            this@ScenarioTree2._nodes.add(this)
            this@ScenarioTree2.lazyCache.invalidate()
            parent?._children?.add(this)
        }

        fun isActive(): Boolean = outputActions.isNotEmpty()
        fun isPassive(): Boolean = outputActions.isEmpty()

        override fun toString(): String {
            return "Node(id=$id, parent=${parent?.id}, children=${children.map { it.id }}, element=$element)"
        }
    }

    fun addScenario(scenario: PositiveScenario2) {
        val root = this.root ?: Node(
            element = ScenarioElement2(
                inputAction = InputAction2(InputEvent("INIT"), InputValues.empty()),
                outputActions = emptyList()
            ),
            parent = null
        )

        var current = root
        var isAnyoneCreated = false
        meow@ for (element in scenario.elements) {
            if (isTrie) {
                for (child in current.children) {
                    if (child.inputAction == element.inputAction) {
                        check(child.element == element) {
                            println("current = $current")
                            println("element = $element")
                            println("child = $child")
                            "ScenarioTree is not deterministic!"
                        }
                        current = child
                        element.nodeId = current.id
                        continue@meow
                    }
                }
            }
            current = Node(element, current)
            element.nodeId = current.id
            isAnyoneCreated = true
        }

        // if (isAnyoneCreated) {
        _scenarios.add(scenario)
        lazyCache.invalidate()
        // } else {
        //     check(isTrie)
        //     // println("[!] No new nodes were inserted into the NegativeScenarioTree")
        // }

        check(activeVertices.size + passiveVertices.size + 1 == size) // TODO: remove
    }

    // Note: all property-like functions are one-based and one-valued

    override fun parent(v: Int): Int = nodes[v - 1].parent?.id ?: 0

    // override fun previousActive(v: Int): Int = nodes[v - 1].previousActive?.id ?: 0
    override fun inputEvent(v: Int): Int = inputEvents.indexOf(nodes[v - 1].inputEvent) + 1

    // override fun outputEvent(v: Int): Int = outputEvents.indexOf(nodes[v - 1].outputEvent) + 1
    override fun inputNumber(v: Int): Int = uniqueInputs.indexOf(nodes[v - 1].inputValues) + 1

    // override fun outputNumber(v: Int): Int = uniqueOutputs.indexOf(nodes[v - 1].outputValues) + 1
    override fun inputValue(v: Int, x: Int): Boolean = nodes[v - 1].inputValues[x - 1]
    // override fun outputValue(v: Int, z: Int): Boolean = nodes[v - 1].outputValues[z - 1]

    override fun toString(): String {
        return "ScenarioTree(size=$size, scenarios=${scenarios.size}, inputEvents=$inputEvents, outputEvents=$outputEvents, inputNames=$inputNames, outputNames=$outputNames)"
    }

    companion object {
        fun fromScenarios(
            scenarios: List<PositiveScenario2>,
            inputNames: List<String>,
            outputNames: List<String>,
            inputEvents: List<InputEvent>? = null,
            outputEvents: List<OutputEvent>? = null,
            isTrie: Boolean = true
        ): ScenarioTree2 {
            return ScenarioTree2(
                inputEvents = inputEvents ?: scenarios.flatMap { scenario ->
                    scenario.elements.map { element -> element.inputAction.event }
                }.distinct(),
                outputEvents = outputEvents ?: scenarios.flatMap { scenario ->
                    scenario.elements.flatMap { element ->
                        element.outputActions.map { outputAction -> outputAction.event }
                    }
                }.distinct(),
                inputNames = inputNames,
                outputNames = outputNames,
                isTrie = isTrie
            ).apply {
                scenarios.forEach(::addScenario)
            }
        }

        fun fromFile(
            file: File,
            inputNames: List<String>,
            outputNames: List<String>,
            inputEvents: List<InputEvent>? = null,
            outputEvents: List<OutputEvent>? = null,
            isTrie: Boolean = true
        ): ScenarioTree2 {
            return fromScenarios(
                scenarios = PositiveScenario2.fromFile(
                    file
                ),
                inputNames = inputNames,
                outputNames = outputNames,
                inputEvents = inputEvents,
                outputEvents = outputEvents,
                isTrie = isTrie
            )
        }
    }
}
