@file:Suppress("MemberVisibilityCanBePrivate")

package ru.ifmo.fbsat.core.scenario.negative

import com.github.lipen.lazycache.LazyCache
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OldScenarioTreeInterface
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import java.io.File

@Deprecated("this is old")
class OldNegativeScenarioTree(
    override val inputEvents: List<InputEvent>,
    override val outputEvents: List<OutputEvent>,
    override val inputNames: List<String>,
    override val outputNames: List<String>,
    private val isTrie: Boolean = true
) : OldScenarioTreeInterface {
    private val lazyCache = LazyCache()
    private val _negativeScenarios: MutableList<NegativeScenario> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    val negativeScenarios: List<NegativeScenario> = _negativeScenarios
    val nodes: List<Node> = _nodes
    val root: Node?
        get() = nodes.firstOrNull()
    override val size: Int
        get() = nodes.size

    // Note: all public lists are zero-based

    override val uniqueInputs: List<InputValues> by lazyCache {
        nodes.asSequence().drop(1).map { it.inputValues }.toSet().toList()
    }
    override val uniqueOutputs: List<OutputValues> by lazyCache {
        nodes.asSequence().drop(1).map { it.outputValues }.toSet().toList()
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

    inner class Node(
        val element: ScenarioElement,
        val parent: Node?,
        var isTerminal: Boolean = false
    ) {
        private val _children: MutableList<Node> = mutableListOf()

        val id: Int = this@OldNegativeScenarioTree.size + 1 // Note: one-based
        val children: List<Node> = _children
        val previousActive: Node? = if (parent?.element?.outputEvent != null) parent else parent?.previousActive
        val loopBacks: MutableSet<Node> = mutableSetOf()
        var isLoopBack: Boolean = false
        val inputAction: InputAction = element.inputAction
        val outputAction: OutputAction = element.outputAction
        val inputEvent: InputEvent? = inputAction.event
        val inputValues: InputValues = inputAction.values
        val outputEvent: OutputEvent? = outputAction.event
        val outputValues: OutputValues = outputAction.values

        init {
            require(!isTerminal) { "Terminal nodes are not supported yet" }

            this@OldNegativeScenarioTree._nodes.add(this)
            this@OldNegativeScenarioTree.lazyCache.invalidate()
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

            val label = """$idStr ${element.outputEvent?.name ?: "ε"} @${element.ceState}"""
            val tableBody = """
                <TR><TD align="center" colspan="2">$label</TD></TR>
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
                    .zip(element.inputValues.values)
                    .joinToString("") { (name, value) ->
                        """${if (!value) "~" else ""}$name\l"""
                    }
                """${parent.id} -> $id [label="${element.inputEvent?.name ?: "ε"}\l$label"]"""
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
            element = ScenarioElement(
                InputAction(
                    event = null,
                    values = InputValues.empty()
                ),
                OutputAction(
                    event = null,
                    values = OutputValues.zeros(negativeScenario.elements.first().outputValues.values.size)
                )
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

    // Note: all property-like functions are one-based and one-valued

    override fun parent(v: Int): Int = nodes[v - 1].parent?.id ?: 0
    override fun previousActive(v: Int): Int = nodes[v - 1].previousActive?.id ?: 0
    override fun inputEvent(v: Int): Int = inputEvents.indexOf(nodes[v - 1].inputEvent) + 1
    override fun outputEvent(v: Int): Int = outputEvents.indexOf(nodes[v - 1].outputEvent) + 1
    override fun inputNumber(v: Int): Int = uniqueInputs.indexOf(nodes[v - 1].inputValues) + 1
    override fun outputNumber(v: Int): Int = uniqueOutputs.indexOf(nodes[v - 1].outputValues) + 1
    override fun inputValue(v: Int, x: Int): Boolean = nodes[v - 1].inputValues[x - 1]
    override fun outputValue(v: Int, z: Int): Boolean = nodes[v - 1].outputValues[z - 1]
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
        fun fromFile(
            file: File,
            inputEvents: List<InputEvent>,
            outputEvents: List<OutputEvent>,
            inputNames: List<String>,
            outputNames: List<String>,
            isTrie: Boolean = true
        ): OldNegativeScenarioTree {
            val negativeScenarios: List<NegativeScenario> =
                NegativeScenario.fromFile(
                    file = file,
                    inputEvents = inputEvents,
                    outputEvents = outputEvents,
                    inputNames = inputNames,
                    outputNames = outputNames
                )
            return OldNegativeScenarioTree(
                inputEvents = inputEvents,
                outputEvents = outputEvents,
                inputNames = inputNames,
                outputNames = outputNames,
                isTrie = isTrie
            ).apply {
                negativeScenarios.forEach(::addNegativeScenario)
            }
        }
    }
}
