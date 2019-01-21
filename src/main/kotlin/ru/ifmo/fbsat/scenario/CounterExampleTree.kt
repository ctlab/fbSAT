package ru.ifmo.fbsat.scenario

import ru.ifmo.fbsat.utils.LazyCache

class CounterExampleTree(
    counterExamples: List<CounterExample>,
    inputNames: List<String>? = null,
    outputNames: List<String>? = null,
    private val isTrie: Boolean = true
) {
    private val lazyCache = LazyCache()
    private val _counterExamples: MutableList<CounterExample> = mutableListOf()
    private val _inputNames: List<String>? = inputNames
    private val _outputNames: List<String>? = outputNames
    private val _nodes: MutableList<Node> = mutableListOf()
    private val nodes: List<Node> = _nodes
    private val root: Node?
        get() = nodes.firstOrNull()

    val counterExamples: List<CounterExample> = _counterExamples

    val size
        get() = nodes.size

    val rootElement: ScenarioElement?
        get() = root?.element

    // Note: all public lists are zero-based
    val inputEvents: List<String> by lazyCache {
        nodes.asSequence().map { it.element.inputEvent }.filter { it != "" }.distinct().sorted().toList()
    }
    val outputEvents: List<String> by lazyCache {
        nodes.asSequence().mapNotNull { it.element.outputEvent }.distinct().sorted().toList()
    }
    val uniqueInputs: List<String> by lazyCache {
        nodes.asSequence().map { it.element.inputValues }.filter { it != "" }.distinct().sorted().toList()
    }
    val uniqueOutputs: List<String> by lazyCache {
        nodes.asSequence().map { it.element.outputValues }.filter { it != "" }.distinct().sorted().toList()
    }
    val inputNames: List<String> by lazyCache {
        _inputNames ?: uniqueInputs.first().indices.map { i -> "x${i + 1}" }
    }
    val outputNames: List<String> by lazyCache {
        _outputNames ?: uniqueOutputs.first().indices.map { i -> "z${i + 1}" }
    }
    val activeVertices: List<Int> by lazyCache {
        nodes.asSequence()
            .drop(1) // without root
            .filter { it.element.outputEvent != null }
            .map { it.id + 1 }
            .toList()
    }
    val passiveVertices: List<Int> by lazyCache {
        nodes.asSequence()
            .drop(1) // without root
            .filter { it.element.outputEvent == null }
            .map { it.id + 1 }
            .toList()
    }
    val verticesWithLoops: List<Int> by lazyCache {
        nodes.asSequence()
            .filter { it.loopBack != null }
            .map { it.id + 1 }
            .toList()
    }
    val activeVerticesEU: Map<Pair<Int, Int>, List<Int>> by lazyCache {
        activeVertices.groupBy { this.inputEvent(it) to this.inputNumber(it) }
    }
    val passiveVerticesEU: Map<Pair<Int, Int>, List<Int>> by lazyCache {
        passiveVertices.groupBy { this.inputEvent(it) to this.inputNumber(it) }
    }

    init {
        counterExamples.forEach(this::addCounterExample)
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
        val parent: Node?,
        var loopBack: Node? = null,
        var isTerminal: Boolean = false
    ) {
        private val _children: MutableList<Node> = mutableListOf()

        val id: Int = this@CounterExampleTree.size  // Note: zero-based
        val children: List<Node> = _children
        val previousActive: Node? = if (parent?.element?.outputEvent != null) parent else parent?.previousActive

        init {
            require(!(loopBack != null && isTerminal)) {
                "Node can't have a back-edge and be a terminal at the same time"
            }
            this@CounterExampleTree._nodes.add(this)
            parent?._children?.add(this)
        }

        override fun toString(): String {
            return "Node(id=$id, parent=${parent?.id}, previousActive=${previousActive?.id}, loopBack=${loopBack?.id}, isTerminal=$isTerminal, children=${children.map { it.id }}, element=$element)"
        }
    }

    fun addCounterExample(counterExample: CounterExample) {
        val root = this.root ?: Node(
            ScenarioElement(
                inputEvent = "",
                inputValues = "",
                outputEvent = "INITO",
                outputValues = "0".repeat(counterExample.elements.first().outputValues.length)
            ),
            null
        )

        var current = root
        var loopBack: Node? = null
        meow@ for ((i, element) in counterExample.elements.withIndex()) {
            if (isTrie) {
                for (child in current.children) {
                    if (child.element.inputEvent == element.inputEvent &&
                        child.element.inputValues == element.inputValues
                    ) {
                        current = child
                        if (i + 1 == counterExample.loopPosition)
                            loopBack = current
                        continue@meow
                    }
                }
            }
            current = Node(element, current)
            if (i + 1 == counterExample.loopPosition)
                loopBack = current
        }

//        if (counterExample.loopPosition != null) {
//            current.loopBack = nodes[counterExample.loopPosition - 1]
//        } else {
//            current.isTerminal = true
//        }
        current.loopBack = loopBack

        _counterExamples.add(counterExample)
        lazyCache.invalidate()
        require(activeVertices.size + passiveVertices.size + 1 == size) // TODO: remove
    }

    // Note: all property-like functions are one-based and one-valued

    fun parent(v: Int) = nodes[v - 1].parent?.run { id + 1 } ?: 0
    fun previousActive(v: Int) = nodes[v - 1].previousActive?.run { id + 1 } ?: 0
    fun inputEvent(v: Int) = inputEvents.indexOf(nodes[v - 1].element.inputEvent) + 1
    fun outputEvent(v: Int) = outputEvents.indexOf(nodes[v - 1].element.outputEvent) + 1
    fun inputNumber(v: Int) = uniqueInputs.indexOf(nodes[v - 1].element.inputValues) + 1
    fun outputNumber(v: Int) = uniqueOutputs.indexOf(nodes[v - 1].element.outputValues) + 1

    fun inputValue(v: Int, x: Int): Boolean =
        when (val c = nodes[v - 1].element.inputValues[x - 1]) {
            '1' -> true
            '0' -> false
            else -> throw IllegalStateException("Character $c for x = $x is neither '1' nor '0'")
        }

    fun outputValue(v: Int, z: Int): Boolean =
        when (val c = nodes[v - 1].element.outputValues[z - 1]) {
            '1' -> true
            '0' -> false
            else -> throw IllegalStateException("Character $c is neither '1' nor '0'")
        }

    fun loopBack(v: Int): Int = nodes[v - 1].loopBack?.run { id + 1 } ?: 0

    override fun toString(): String {
        return "CounterExampleTree(size=$size, counterexamples=${counterExamples.size}, inputEvents=$inputEvents, outputEvents=$outputEvents, inputNames=$inputNames, outputNames=$outputNames, uniqueInputs=$uniqueInputs, uniqueOutputs=$uniqueOutputs)"
    }
}
