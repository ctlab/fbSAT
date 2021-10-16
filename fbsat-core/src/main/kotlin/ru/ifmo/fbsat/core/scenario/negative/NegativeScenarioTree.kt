package ru.ifmo.fbsat.core.scenario.negative

import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.scenario.addGenericScenario

class NegativeScenarioTree(
    override val inputEvents: List<InputEvent>,
    override val outputEvents: List<OutputEvent>,
    override val inputNames: List<String>,
    override val outputNames: List<String>,
    initialOutputValues: OutputValues? = null,
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
        Node(
            element = ScenarioElement(
                InputAction(
                    event = null,
                    values = InputValues.empty()
                ),
                OutputAction(
                    event = null,
                    values = initialOutputValues ?: OutputValues.zeros(outputNames.size)
                )
            ),
            parent = null
        )
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
                Node(element, parent = current).also { newNode ->
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

        check(loopBack != null) { "Weird, but loopBack is null." }

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
