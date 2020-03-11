@file:Suppress("MemberVisibilityCanBePrivate")

package ru.ifmo.fbsat.core.scenario.positive

import com.github.lipen.lazycache.LazyCache
import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.automaton.OutputValues
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.ScenarioTreeInterface
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.toBinaryString
import java.io.File

class ScenarioTree(
    override val inputEvents: List<InputEvent>,
    override val outputEvents: List<OutputEvent>,
    override val inputNames: List<String>,
    override val outputNames: List<String>,
    private val isTrie: Boolean = true
) : ScenarioTreeInterface {
    private val lazyCache = LazyCache()
    private val _scenarios: MutableList<PositiveScenario> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    val scenarios: List<PositiveScenario> = _scenarios
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
        nodes.asSequence().drop(1).map { it.outputValues }.toSet().toList().sortedBy { it.values.toBinaryString() }
    }
    /**
     * List of **active** vertices, i.e. vertices with **non-null** output event.
     * The root is excluded explicitly.
     */
    val activeVertices: List<Int> by lazyCache {
        nodes.asSequence()
            .drop(1) // without root
            .filter { it.outputEvent != null }
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
            .filter { it.outputEvent == null }
            .map { it.id }
            .toList()
    }
    val activeVerticesEU: Map<Pair<Int, Int>, List<Int>> by lazyCache {
        activeVertices.groupBy { inputEvent(it) to inputNumber(it) }
    }
    val passiveVerticesEU: Map<Pair<Int, Int>, List<Int>> by lazyCache {
        passiveVertices.groupBy { inputEvent(it) to inputNumber(it) }
    }

    inner class Node(
        val element: ScenarioElement,
        val parent: Node?
    ) {
        private val _children: MutableList<Node> = mutableListOf()

        val id: Int = this@ScenarioTree.size + 1 // Note: one-based
        val children: List<Node> = _children
        val previousActive: Node? = if (parent?.outputEvent != null) parent else parent?.previousActive
        val inputAction: InputAction = element.inputAction
        val outputAction: OutputAction = element.outputAction
        val inputEvent: InputEvent? = inputAction.event
        val inputValues: InputValues = inputAction.values
        val outputEvent: OutputEvent? = outputAction.event
        val outputValues: OutputValues = outputAction.values

        init {
            if (id > 1) {
                require(element.inputEvent == null || element.inputEvent in this@ScenarioTree.inputEvents) {
                    "Unexpected input event '${element.inputEvent}'"
                }
                require(element.outputEvent == null || element.outputEvent in this@ScenarioTree.outputEvents) {
                    "Unexpected output event '${element.outputEvent}'"
                }
                require(element.inputValues.values.size == this@ScenarioTree.inputNames.size) {
                    "Unexpected number of input values (element: ${element.inputValues.values.size}, tree: ${this@ScenarioTree.inputNames.size})"
                }
                require(element.outputValues.values.size == this@ScenarioTree.outputNames.size) {
                    "Unexpected number of output values (element: ${element.outputValues.values.size}, tree: ${this@ScenarioTree.outputNames.size})"
                }
            }

            this@ScenarioTree._nodes.add(this)
            this@ScenarioTree.lazyCache.invalidate()
            parent?._children?.add(this)
        }

        override fun toString(): String {
            return "Node(id=$id, parent=${parent?.id}, previousActive=${previousActive?.id}, children=${children.map { it.id }}, element=$element)"
        }
    }

    fun addScenario(scenario: PositiveScenario) {
        val root = this.root ?: Node(
            element = ScenarioElement(
                InputAction(
                    event = null,
                    values = InputValues.empty()
                ),
                OutputAction(
                    event = null,
                    // event = OutputEvent("INITO"),
                    values = Globals.INITIAL_OUTPUT_VALUES
                )
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
    override fun previousActive(v: Int): Int = nodes[v - 1].previousActive?.id ?: 0
    override fun inputEvent(v: Int): Int = inputEvents.indexOf(nodes[v - 1].inputEvent) + 1
    override fun outputEvent(v: Int): Int = outputEvents.indexOf(nodes[v - 1].outputEvent) + 1
    override fun inputNumber(v: Int): Int = uniqueInputs.indexOf(nodes[v - 1].inputValues) + 1
    override fun outputNumber(v: Int): Int = uniqueOutputs.indexOf(nodes[v - 1].outputValues) + 1
    override fun inputValue(v: Int, x: Int): Boolean = nodes[v - 1].inputValues[x - 1]
    override fun outputValue(v: Int, z: Int): Boolean = nodes[v - 1].outputValues[z - 1]

    override fun toString(): String {
        return "ScenarioTree(size=$size, scenarios=${scenarios.size}, inputEvents=$inputEvents, outputEvents=$outputEvents, inputNames=$inputNames, outputNames=$outputNames)"
    }

    companion object {
        fun fromScenarios(
            scenarios: List<PositiveScenario>,
            inputNames: List<String>,
            outputNames: List<String>,
            inputEvents: List<InputEvent>? = null,
            outputEvents: List<OutputEvent>? = null,
            isTrie: Boolean = true
        ): ScenarioTree {
            return ScenarioTree(
                inputEvents = inputEvents ?: scenarios.flatMap { scenario ->
                    scenario.elements.mapNotNull { element -> element.inputAction.event }
                }.distinct(),
                outputEvents = outputEvents ?: scenarios.flatMap { scenario ->
                    scenario.elements.mapNotNull { element -> element.outputAction.event }
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
        ): ScenarioTree {
            val scenarios: List<PositiveScenario> = PositiveScenario.fromFile(file)
            return fromScenarios(
                scenarios = scenarios,
                inputNames = inputNames,
                outputNames = outputNames,
                inputEvents = inputEvents,
                outputEvents = outputEvents,
                isTrie = isTrie
            )
        }
    }
}
