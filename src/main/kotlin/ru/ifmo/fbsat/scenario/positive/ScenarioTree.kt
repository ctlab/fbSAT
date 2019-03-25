package ru.ifmo.fbsat.scenario.positive

import ru.ifmo.fbsat.scenario.ScenarioElement
import ru.ifmo.fbsat.utils.LazyCache

class ScenarioTree(
    scenarios: List<PositiveScenario>,
    inputNames: List<String>? = null,
    outputNames: List<String>? = null,
    private val isTrie: Boolean = true
) {
    private val lazyCache = LazyCache()
    private val _scenarios: MutableList<PositiveScenario> = mutableListOf()
    private val _inputNames: List<String>? = inputNames
    private val _outputNames: List<String>? = outputNames
    private val _nodes: MutableList<Node> = mutableListOf()
    private val nodes: List<Node> = _nodes
    private val root: Node?
        get() = nodes.firstOrNull()

    val scenarios: List<PositiveScenario> = _scenarios

    val size
        get() = nodes.size

    val rootElement: ScenarioElement?
        get() = root?.element

    // Note: all public lists are zero-based
    val inputEvents: List<String> by lazyCache {
        nodes.asSequence().map { it.element.inputEvent }.filter { it != "" }.toSet().sorted()
    }
    val outputEvents: List<String> by lazyCache {
        nodes.asSequence().mapNotNull { it.element.outputEvent }.toSet().sorted()
    }
    val uniqueInputs: List<String> by lazyCache {
        nodes.asSequence().map { it.element.inputValues }.filter { it != "" }.toSet().sorted()
    }
    val uniqueOutputs: List<String> by lazyCache {
        nodes.asSequence().map { it.element.outputValues }.filter { it != "" }.toSet().sorted()
    }
    val inputNames: List<String> by lazyCache {
        _inputNames ?: uniqueInputs.first().indices.map { "x${it + 1}" }
    }
    val outputNames: List<String> by lazyCache {
        _outputNames ?: uniqueOutputs.first().indices.map { "z${it + 1}" }
    }
    /**
     * List of **all** vertices (including root).
     */
    val allVertices: List<Int> by lazyCache {
        nodes.asSequence()
            .map(Node::id)
            .toList()
    }
    /**
     * List of all vertices **excluding root**.
     */
    val verticesWithoutRoot: List<Int> by lazyCache {
        nodes.asSequence()
            .drop(1)
            .map(Node::id)
            .toList()
    }
    /**
     * List of **active** vertices, i.e. vertices with **non-null** output event.
     * The root is excluded explicitly.
     */
    val activeVertices: List<Int> by lazyCache {
        nodes.asSequence()
            .drop(1) // without root
            .filter { it.element.outputEvent != null }
            .map(Node::id)
            .toList()
    }
    /**
     * List of **passive** vertices, i.e. vertices with **null** (aka empty/epsilon) output event.
     * The root is excluded explicitly.
     */
    val passiveVertices: List<Int> by lazyCache {
        nodes.asSequence()
            .drop(1) // without root
            .filter { it.element.outputEvent == null }
            .map(Node::id)
            .toList()
    }
    val activeVerticesEU: Map<Pair<Int, Int>, List<Int>> by lazyCache {
        activeVertices.groupBy { this.inputEvent(it) to this.inputNumber(it) }
    }
    val passiveVerticesEU: Map<Pair<Int, Int>, List<Int>> by lazyCache {
        passiveVertices.groupBy { this.inputEvent(it) to this.inputNumber(it) }
    }

    init {
        scenarios.forEach(this::addScenario)
        if (inputNames != null) require(inputNames.size == uniqueInputs.first().length)
        if (outputNames != null) require(outputNames.size == uniqueOutputs.first().length)
    }

    init {
        println("[.] $this")
        val n = 5
        println("[.] First $n nodes:")
        for (node in nodes.take(n))
            println("[.] $node")
    }

    private inner class Node(
        val element: ScenarioElement,
        val parent: Node?
    ) {
        private val _children: MutableList<Node> = mutableListOf()

        val id: Int = this@ScenarioTree.size + 1 // Note: one-based
        val children: List<Node> = _children
        val previousActive: Node? = if (parent?.element?.outputEvent != null) parent else parent?.previousActive

        init {
            this@ScenarioTree._nodes.add(this)
            parent?._children?.add(this)
        }

        override fun toString(): String {
            return "Node(id=$id, parent=${parent?.id}, previousActive=${previousActive?.id}, children=${children.map { it.id }}, element=$element)"
        }
    }

    fun addScenario(scenario: PositiveScenario) {
        val root = this.root ?: Node(
            ScenarioElement(
                inputEvent = "",
                inputValues = "",
                outputEvent = "INITO",
                outputValues = "0".repeat(outputNames.size)
            ),
            null
        )

        var current = root
        var isAnyoneCreated = false
        meow@ for (element in scenario.elements) {
            if (isTrie) {
                for (child in current.children) {
                    if (child.element.inputEvent == element.inputEvent &&
                        child.element.inputValues == element.inputValues
                    ) {
                        check(child.element == element) { "ScenarioTree is not deterministic!" }
                        current = child
                        continue@meow
                    }
                }
            }
            current = Node(element, current)
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

    fun parent(v: Int) = nodes[v - 1].parent?.id ?: 0
    fun previousActive(v: Int) = nodes[v - 1].previousActive?.id ?: 0
    fun inputEvent(v: Int) = inputEvents.indexOf(nodes[v - 1].element.inputEvent) + 1
    fun outputEvent(v: Int) = outputEvents.indexOf(nodes[v - 1].element.outputEvent) + 1
    fun inputNumber(v: Int) = uniqueInputs.indexOf(nodes[v - 1].element.inputValues) + 1
    fun outputNumber(v: Int) = uniqueOutputs.indexOf(nodes[v - 1].element.outputValues) + 1

    fun inputValue(v: Int, x: Int): Boolean =
        when (val c = nodes[v - 1].element.inputValues[x - 1]) {
            '1' -> true
            '0' -> false
            else -> error("Character $c for v = $v, x = $x is neither '1' nor '0'")
        }

    fun outputValue(v: Int, z: Int): Boolean =
        when (val c = nodes[v - 1].element.outputValues[z - 1]) {
            '1' -> true
            '0' -> false
            else -> error("Character $c for v = $v, z = $z is neither '1' nor '0'")
        }

    override fun toString(): String {
        return "ScenarioTree(size=$size, scenarios=${scenarios.size}, inputEvents=$inputEvents, outputEvents=$outputEvents, inputNames=$inputNames, outputNames=$outputNames)"
    }
}
