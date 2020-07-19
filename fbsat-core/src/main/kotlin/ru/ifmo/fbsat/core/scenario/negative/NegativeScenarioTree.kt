package ru.ifmo.fbsat.core.scenario.negative

import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.scenario.addGenericScenario
import ru.ifmo.fbsat.core.scenario.auxScenarioElement

class NegativeScenarioTree(
    override val inputEvents: List<InputEvent>,
    override val outputEvents: List<OutputEvent>,
    override val inputNames: List<String>,
    override val outputNames: List<String>,
    override val isTrie: Boolean = true
) : ScenarioTree<NegativeScenario, NegativeScenarioTree.Node> {
    private val _scenarios: MutableList<NegativeScenario> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    override val scenarios: List<NegativeScenario> = _scenarios
    override val nodes: List<Node> = _nodes
    override val root: Node
        get() = nodes.first()

    init {
        // Add the root
        _nodes.add(Node(element = auxScenarioElement, parent = null))
    }

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
                        println("[${index + 1}/${scenario.elements.size}] loopBack now = $loopBack")
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
                        println("[${index + 1}/${scenario.elements.size}] loopBack now = $loopBack")
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
        val isTerminal: Boolean = false
    ) : ScenarioTree.Node {
        private val _children: MutableList<Node> = mutableListOf()

        override val id: Int = this@NegativeScenarioTree.size + 1 // one-based
        override val children: List<Node> = _children

        val loopBacks: MutableSet<Node> = mutableSetOf()
        internal var isLoopBack: Boolean = false

        init {
            parent?._children?.add(this)
            this@NegativeScenarioTree._nodes.add(this)
        }
    }
}
