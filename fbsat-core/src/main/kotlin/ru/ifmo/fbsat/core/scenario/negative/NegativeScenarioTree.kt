package ru.ifmo.fbsat.core.scenario.negative

import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.scenario.addGenericScenario
import ru.ifmo.fbsat.core.scenario.auxScenarioElement
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.NegativeTreeOptimizations
import java.io.File

class NegativeScenarioTree(
    override val inputEvents: List<InputEvent>,
    override val outputEvents: List<OutputEvent>,
    override val inputNames: List<String>,
    override val outputNames: List<String>,
    override val isTrie: Boolean = false,
) : ScenarioTree<NegativeScenario, NegativeScenarioTree.Node> {
    private val _scenarios: MutableList<NegativeScenario> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    override val scenarios: List<NegativeScenario> = _scenarios
    override val nodes: List<Node> = _nodes
    override val root: Node
        get() = nodes.first()

    init {
        // Create the root (auto-added to _nodes)
        Node(element = auxScenarioElement, parent = null, -1)
    }

    fun toGraphvizString(heat: Map<Int, Set<Int>> = mapOf(), nullNode: Int? = null) =
"""digraph L {
${ run {
    val max = heat.mapNotNull { it.value.maxOrNull() }.maxOrNull() ?: 0
    val color = heat
        .mapNotNull { it.value.minOrNull()?.let { min -> it.key to (min to it.value.size) } }
        .map {
            val (min, count) = it.second
            it.first to 100 - 100 * count / (max - min + 1)
        }
        .onEach { check(it.second in 0..100) }
        .toMap()
    nodes.joinToString("\n") {
        "qq${it.id}[label=\"${
            it.element.outputEvent?.name ?: "ε"
        }[${
            it.element.outputValues.values.joinToString("") { b ->
                if (b) "1" else "0"
            }
        }]\", style=filled, fillcolor=\"${
            if (nullNode == null || it.id != nullNode) "grey${ color[it.id] ?: 100 }" else "red"
        }\", fontcolor=\"${
            if (color[it.id] ?: 100 > 51) "black" else "white"
        }\"]"
    }
}
}
${
    nodes.mapNotNull { node ->
        node.parent?.let { it to node }
    }.joinToString("\n") {
        "qq${it.first.id}->qq${it.second.id}[label=\"${
            it.second.element.inputEvent?.name ?: "ε"
        }[${
            it.second.element.inputValues.values.joinToString("") { b ->
                if (b) "1" else "0"
            }
        }]\"]"
    }
}
${
    nodes.flatMap { node ->
        node.loopBacks.map { node to it }
    }.joinToString("\n") {
        "qq${it.first.id}->qq${it.second.id}[style=dashed]"
    }
}
}""".trimIndent()

    fun dump(dir: File, name: String = "negative_tree", heat: Map<Int, Set<Int>> = mapOf(), nullNode: Int? = null) {
        dir.mkdirs()
        dumpGv(dir.resolve("$name.dot"), heat, nullNode)
    }

    fun dumpGv(file: File, heat: Map<Int, Set<Int>> = mapOf(), nullNode: Int? = null) {
        file.printWriter().use {
            it.println(toGraphvizString(heat, nullNode))
        }
        Runtime.getRuntime().exec("dot -Tpng -O $file")
    }

    constructor(
        positiveScenarioTree: ScenarioTree<*, *>,
        isTrie: Boolean = false,
    ) : this(
        inputEvents = positiveScenarioTree.inputEvents,
        outputEvents = positiveScenarioTree.outputEvents,
        inputNames = positiveScenarioTree.inputNames,
        outputNames = positiveScenarioTree.outputNames,
        isTrie = isTrie
    )

    fun loopBacks(v: Int): List<Int> = nodes[v - 1].loopBacks.map { it.id }

    @Suppress("DuplicatedCode")
    fun addScenario(scenario: NegativeScenario) {
        require(scenario.elements.isNotEmpty())

        lateinit var last: Node
        var loopBack: Node? = null

        addGenericScenario(
            scenario,
            sameNode = { index, _, child ->
                child.also { newNode ->
                    if (index + 1 == scenario.loopPosition) {
                        check(loopBack == null) { "Cannot override loopBack = $loopBack to $newNode" }
                        loopBack = newNode
                        // log.debug { "[${index + 1}/${scenario.elements.size}] [same] loopBack now = $loopBack" }
                    }
                    last = newNode
                }
            },
            newNode = { index, element, current ->
                Node(element, parent = current, height = index).also { newNode ->
                    element.nodeId = newNode.id
                    if (index + 1 == scenario.loopPosition) {
                        check(loopBack == null) { "Cannot override loopBack = $loopBack to $newNode" }
                        loopBack = newNode
                        // log.debug { "[${index + 1}/${scenario.elements.size}] [new] loopBack now = $loopBack" }
                    }
                    last = newNode
                }
            }
        )

        check(loopBack != null || Globals.LOOPLESS_COUNTER_EXAMPLES) { "Weird, but loopBack is null." }

        if (loopBack != null) {
            loopBack!!.isLoopBack = true
            check(!(loopBack!!.id in last.loopBacks.map { it.id } && loopBack!! !in last.loopBacks))
            last.loopBacks.add(loopBack!!)
        }

        _scenarios.add(scenario)
    }

    inner class Node(
        override val element: ScenarioElement,
        override val parent: Node?,
        val height: Int,
    ) : ScenarioTree.Node<Node> {
        private val _children: MutableList<Node> = mutableListOf()

        override val id: Int = this@NegativeScenarioTree.size + 1 // one-based
        override val children: List<Node> = _children

        val loopBacks: MutableSet<Node> = mutableSetOf()
        internal var isLoopBack: Boolean = false

        init {
            parent?._children?.add(this)
            this@NegativeScenarioTree._nodes.add(this)
        }

        override fun toString(): String {
            return "Node(id=$id, parent=${parent?.id}, children=${children.map { it.id }}, element=$element)"
        }
    }
}

@Deprecated("Migrate to NegativeScenarioTree, pretty please")
fun NegativeScenarioTree.toOld(): OldNegativeScenarioTree {
    return OldNegativeScenarioTree(
        inputEvents = inputEvents,
        outputEvents = outputEvents,
        inputNames = inputNames,
        outputNames = outputNames,
        isTrie = isTrie
    ).also {
        for (scenario in scenarios) {
            it.addNegativeScenario(scenario)
        }
    }
}
