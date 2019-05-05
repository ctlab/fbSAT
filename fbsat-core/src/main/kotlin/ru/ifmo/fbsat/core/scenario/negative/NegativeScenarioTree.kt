package ru.ifmo.fbsat.core.scenario.negative

import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.utils.LazyCache
import ru.ifmo.fbsat.core.utils.ObservableMutableList
import java.io.File

class NegativeScenarioTree(
    negativeScenarios: List<NegativeScenario>,
    // TODO: forbid passing null input/output names
    inputNames: List<String>? = null,
    outputNames: List<String>? = null,
    private val isTrie: Boolean = true
) {
    private val _negativeScenarios = ObservableMutableList<NegativeScenario>()
    private val lazyCache = LazyCache(_negativeScenarios)
    private val _inputNames: List<String>? = inputNames
    private val _outputNames: List<String>? = outputNames
    private val _nodes: MutableList<Node> = mutableListOf()
    private val nodes: List<Node> = _nodes
    private val root: Node?
        get() = nodes.firstOrNull()

    val negativeScenarios: List<NegativeScenario> = _negativeScenarios

    val size: Int
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
            .map { it.id }
            .toList()
    }
    val activeVertices: List<Int> by lazyCache {
        nodes.asSequence()
            .drop(1) // without root
            .filter { it.element.outputEvent != null }
            .map { it.id }
            .toList()
    }
    val passiveVertices: List<Int> by lazyCache {
        nodes.asSequence()
            .drop(1) // without root
            .filter { it.element.outputEvent == null }
            .map { it.id }
            .toList()
    }
    val verticesWithLoops: List<Int> by lazyCache {
        nodes.asSequence()
            .filter { it.loopBacks.isNotEmpty() }
            .map { it.id }
            .toList()
    }
    val activeVerticesEU: Map<Pair<Int, Int>, List<Int>> by lazyCache {
        activeVertices.groupBy { this.inputEvent(it) to this.inputNumber(it) }
    }
    val passiveVerticesEU: Map<Pair<Int, Int>, List<Int>> by lazyCache {
        passiveVertices.groupBy { this.inputEvent(it) to this.inputNumber(it) }
    }

    init {
        negativeScenarios.forEach(this::addNegativeScenario)
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
        var isTerminal: Boolean = false
    ) {
        private val _children: MutableList<Node> = mutableListOf()

        val id: Int = this@NegativeScenarioTree.size + 1 // Note: one-based
        val children: List<Node> = _children
        val previousActive: Node? = if (parent?.element?.outputEvent != null) parent else parent?.previousActive
        val loopBacks: MutableSet<Node> = mutableSetOf()
        var isLoopBack: Boolean = false

        init {
            require(!isTerminal) { "Terminal nodes are not supported yet" }

            this@NegativeScenarioTree._nodes.add(this)
            parent?._children?.add(this)
        }

        fun toGraphvizString(): String {
            val idStr = if (isLoopBack) "&lsaquo;$id&rsaquo;" else "$id"

            val vs = outputNames.indices.joinToString("\n") { i ->
                val name = outputNames[i]
                val oldValue = parent?.element?.outputValues?.get(i) ?: '0'
                val newValue = this.element.outputValues[i]

                if (oldValue != newValue)
                    """<TR><TD align="left"><FONT color="red">$name</FONT></TD><TD><FONT color="red">$newValue</FONT></TD></TR>"""
                else
                    """<TR><TD align="left">$name</TD><TD>$newValue</TD></TR>"""
            }

            val tableBody = """
                <TR><TD align="center" colspan="2">$idStr ${element.outputEvent ?: "ε"} @${element.ceState}</TD></TR>
                <HR/>
                %s
            """.trimIndent().format(vs)

            val html = """
                <TABLE style="rounded" cellborder="0" cellspacing="1">
                %s
                </TABLE>
            """.trimIndent().format(tableBody.prependIndent())

            return "$id [label=<\n${html.prependIndent()}> shape=plaintext]"
        }

        fun getIncomingEdgeGraphvizString(): String {
            return if (parent != null) {
                val label = inputNames
                    .zip(element.inputValues.asIterable())
                    .joinToString("") { (name, value) ->
                        """${if (value == '0') "~" else ""}$name\l"""
                    }
                """${parent.id} -> $id [label="$label"]"""
            } else {
                val label = "init"
                """start -> $id [label="$label"]"""
            }
        }

        fun getLoopBackGraphvizStrings(): List<String> {
            return loopBacks.map { l -> """$id -> ${l.id} [label=" " style=dashed]""" }
        }

        override fun toString(): String {
            return "Node(id=$id, parent=${parent?.id}, previousActive=${previousActive?.id}, loopBacks=${loopBacks.map { it.id }}, isTerminal=$isTerminal, children=${children.map { it.id }}, element=$element)"
        }
    }

    fun addNegativeScenario(negativeScenario: NegativeScenario) {
        val root = this.root ?: Node(
            ScenarioElement(
                inputEvent = "",
                inputValues = "",
                outputEvent = "INITO",
                outputValues = "0".repeat(negativeScenario.elements.first().outputValues.length),
                ceState = negativeScenario.elements.first().ceState
            ),
            parent = null
        )

        var current: Node = root
        var loopBack: Node? = null
        var isAnyoneCreated = false
        meow@ for ((i, element) in negativeScenario.elements.withIndex()) {
            if (isTrie) {
                for (child in current.children) {
                    if (element == child.element) {
                        current = child
                        element.nodeId = current.id
                        if (i + 1 == negativeScenario.loopPosition) {
                            check(loopBack == null) {
                                "loopBack already = $loopBack, but you are trying to override it with current = $current"
                            }
                            loopBack = current
                            // println("[${i + 1}/${negativeScenario.elements.size}] loopBack now(1) = $loopBack")
                        }
                        continue@meow
                    }
                }
            }
            current = Node(element, current)
            isAnyoneCreated = true
            element.nodeId = current.id
            if (i + 1 == negativeScenario.loopPosition) {
                check(loopBack == null) {
                    "loopBack already = $loopBack, but you are trying to override it with current = $current"
                }
                loopBack = current
                // println("[${i + 1}/${negativeScenario.elements.size}] loopBack now(2) = $loopBack")
            }
        }
        check(loopBack != null) { "Weird, but loopBack is null." }

        if (loopBack != null) {
            loopBack.isLoopBack = true
            val last: Node = current
            check(!(loopBack.id in last.loopBacks.map { it.id } && loopBack !in last.loopBacks))
            last.loopBacks.add(loopBack)
        }

        // if (isAnyoneCreated) {
        _negativeScenarios.add(negativeScenario)
        lazyCache.invalidate()
        // } else {
        //     check(isTrie)
        //     // println("[!] No new nodes were inserted into the NegativeScenarioTree")
        // }

        check(activeVertices.size + passiveVertices.size + 1 == size) // TODO: remove
    }

    fun addFromFile(fileNegativeScenarios: File, scenarioTree: ScenarioTree) {
        NegativeScenario.fromFile(
            fileNegativeScenarios,
            scenarioTree.inputEvents,
            scenarioTree.outputEvents,
            scenarioTree.inputNames,
            scenarioTree.outputNames
        ).forEach(::addNegativeScenario)
    }

    fun isEmpty(): Boolean {
        // Note: if you add an empty scenario to an empty tree, then the size will be equal to 1 -
        //   due to added auxiliary root.
        return size == 0
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

    fun isLoopBack(v: Int): Boolean = nodes[v - 1].isLoopBack
    fun isTerminal(v: Int): Boolean = nodes[v - 1].isTerminal
    fun loopBacks(v: Int): List<Int> = nodes[v - 1].loopBacks.map { it.id }

    fun toGraphvizString(): String {
        val fontSettings = """fontname="Source Code Pro,monospace" fontsize="12""""
        val setupBlock = """
            rankdir = LR
            graph [$fontSettings]
            node  [$fontSettings]
            edge  [$fontSettings]
        """.trimIndent()

        val nodesBlock = """
            // States
            start [style=invis]
            %s
        """.trimIndent().format(
            nodes.joinToString("\n") { it.toGraphvizString() }
        )

        val transitionsBlock = """
            // Transitions
            %s

            // Loop-backs
            %s
        """.trimIndent().format(
            nodes.joinToString("\n") { it.getIncomingEdgeGraphvizString() },
            nodes.flatMap { it.getLoopBackGraphvizStrings() }.joinToString("\n")
        )

        val body = "%s\n\n%s\n\n%s".format(
            setupBlock,
            nodesBlock,
            transitionsBlock
        )
        return "digraph {\n${body.prependIndent()}\n}"
    }

    override fun toString(): String {
        return "NegativeScenarioTree(size=$size, negativeScenarios=${negativeScenarios.size}, inputEvents=$inputEvents, outputEvents=$outputEvents, inputNames=$inputNames, outputNames=$outputNames)"
    }

    companion object {
        fun empty(
            inputNames: List<String>,
            outputNames: List<String>,
            isTrie: Boolean = true
        ) = NegativeScenarioTree(
            emptyList(),
            inputNames = inputNames,
            outputNames = outputNames,
            isTrie = isTrie
        )

        fun fromFile(
            file: File,
            inputEvents: List<String>,
            outputEvents: List<String>,
            inputNames: List<String>,
            outputNames: List<String>,
            isTrie: Boolean = true
        ): NegativeScenarioTree {
            val negativeScenarios = NegativeScenario.fromFile(
                file,
                inputEvents,
                outputEvents,
                inputNames,
                outputNames
            )
            return NegativeScenarioTree(
                negativeScenarios,
                inputNames = inputNames,
                outputNames = outputNames,
                isTrie = isTrie
            )
        }
    }
}
