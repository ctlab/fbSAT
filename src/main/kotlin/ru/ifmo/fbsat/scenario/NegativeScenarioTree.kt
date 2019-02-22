package ru.ifmo.fbsat.scenario

import ru.ifmo.fbsat.utils.LazyCache
import java.io.File

class NegativeScenarioTree(
    negativeScenarios: List<NegativeScenario>,
    inputNames: List<String>? = null,
    outputNames: List<String>? = null,
    private val isTrie: Boolean = false
) {
    private val lazyCache = LazyCache()
    private val _counterExamples: MutableList<NegativeScenario> = mutableListOf()
    private val _inputNames: List<String>? = inputNames
    private val _outputNames: List<String>? = outputNames
    private val _nodes: MutableList<Node> = mutableListOf()
    private val nodes: List<Node> = _nodes
    private val root: Node?
        get() = nodes.firstOrNull()

    val counterExamples: List<NegativeScenario> = _counterExamples

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
        _inputNames ?: uniqueInputs.first().indices.map { "x${it + 1}" }
    }
    val outputNames: List<String> by lazyCache {
        _outputNames ?: uniqueOutputs.first().indices.map { "z${it + 1}" }
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
            val idStr = if (isLoopBack) "&laquo;$id&raquo;" else "$id"

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

        val firstElement = negativeScenario.elements.first()
        require(firstElement.inputEvent == "")
        require(firstElement.inputValues == "")
        require(firstElement.outputEvent == null)
        require(firstElement.outputValues == root.element.outputValues)
        firstElement.nodeId = root.id

        var current = root
        var loopBack: Node? = null
        var isAnyoneCreated = false
        // skip first element, because it is the same as root
        meow@ for ((i, element) in negativeScenario.elements.withIndex().drop(1)) {
            if (isTrie) {
                for (child in current.children) {
                    if (child.element == element) {
                        current = child
                        element.nodeId = current.id
                        if (i + 1 == negativeScenario.loopPosition)
                            loopBack = current
                        continue@meow
                    }
                }
            }
            current = Node(element, current)
            isAnyoneCreated = true
            element.nodeId = current.id
            if (i + 1 == negativeScenario.loopPosition)
                loopBack = current
        }
        check(loopBack != null) { "Weird, but loopBack is null." }

        loopBack.isLoopBack = true
        val last = current
        check(!(loopBack.id in last.loopBacks.map { it.id } && loopBack !in last.loopBacks))
        last.loopBacks.add(loopBack)

        // if (isAnyoneCreated) {
            _counterExamples.add(negativeScenario)
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

    // Note: all property-like functions are one-based and one-valued

    fun parent(v: Int) = nodes[v - 1].parent?.id ?: 0
    fun previousActive(v: Int) = nodes[v - 1].previousActive?.id ?: 0
    fun inputEvent(v: Int) = inputEvents.indexOf(nodes[v - 1].element.inputEvent) + 1
    fun outputEvent(v: Int) = outputEvents.indexOf(nodes[v - 1].element.outputEvent) + 1
    fun inputNumber(v: Int) = uniqueInputs.indexOf(nodes[v - 1].element.inputValues) + 1
    fun outputNumber(v: Int) = uniqueOutputs.indexOf(nodes[v - 1].element.outputValues) + 1
    fun uniqueInputNumber(input: String) = uniqueInputs.indexOf(input) + 1

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
        return "NegativeScenarioTree(size=$size, counterexamples=${counterExamples.size}, inputEvents=$inputEvents, outputEvents=$outputEvents, inputNames=$inputNames, outputNames=$outputNames)"
    }

    companion object {
        fun fromFile(
            file: File,
            inputEvents: List<String>,
            outputEvents: List<String>,
            inputNames: List<String>,
            outputNames: List<String>,
            isTrie: Boolean = true
        ): NegativeScenarioTree {
            val counterExamples = NegativeScenario.fromFile(
                file,
                inputEvents,
                outputEvents,
                inputNames,
                outputNames
            )
            return NegativeScenarioTree(
                counterExamples,
                inputNames = inputNames,
                outputNames = outputNames,
                isTrie = isTrie
            )
        }
    }
}
