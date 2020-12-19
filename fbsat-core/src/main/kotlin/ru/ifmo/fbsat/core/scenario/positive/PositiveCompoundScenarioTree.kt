package ru.ifmo.fbsat.core.scenario.positive

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.scenario.CompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.scenario.addGenericScenario
import ru.ifmo.fbsat.core.scenario.auxScenarioElement
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.project

class PositiveCompoundScenarioTree(
    override val M: Int,
    override val modularInputEvents: MultiArray<List<InputEvent>>,
    override val modularOutputEvents: MultiArray<List<OutputEvent>>,
    override val modularInputNames: MultiArray<List<String>>,
    override val modularOutputNames: MultiArray<List<String>>,
    override val isTrie: Boolean = true,
) : CompoundScenarioTree<PositiveScenarioTree, PositiveCompoundScenario, PositiveCompoundScenarioTree.Node>,
    CompoundImpl<PositiveScenarioTree>() {

    init {
        require(modularInputEvents.shape.single() == M)
        require(modularOutputEvents.shape.single() == M)
        require(modularInputNames.shape.single() == M)
        require(modularOutputNames.shape.single() == M)
    }

    override val modular: MultiArray<PositiveScenarioTree> =
        MultiArray.new(M) { (m) ->
            PositiveScenarioTree(
                inputEvents = modularInputEvents[m],
                outputEvents = modularOutputEvents[m],
                inputNames = modularInputNames[m],
                outputNames = modularOutputNames[m],
                isTrie = isTrie
            )
        }

    private val _scenarios: MutableList<PositiveCompoundScenario> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    override val scenarios: List<PositiveCompoundScenario> = _scenarios
    override val nodes: List<Node> = _nodes

    override val size: Int get() = nodes.size
    override val root: Node get() = nodes.first()

    init {
        // Create the root (auto-added to _nodes)
        Node(
            element = CompoundScenarioElement(MultiArray.new(M) { auxScenarioElement }),
            parent = null
        )
    }

    fun addScenario(scenario: PositiveCompoundScenario) {
        addGenericScenario(
            scenario,
            sameNode = { _, _, child -> child },
            newNode = { _, element, current ->
                Node(element, parent = current)
            }
        )
        for (m in 1..M) {
            project(m).addScenario(scenario.project(m))
        }
        _scenarios.add(scenario)
    }

    inner class Node(
        override val element: CompoundScenarioElement,
        override val parent: Node?,
    ) : CompoundScenarioTree.Node<Node>, CompoundImpl<ScenarioTree.Node<*>>() {
        override val M: Int = this@PositiveCompoundScenarioTree.M
        override val id: Int = this@PositiveCompoundScenarioTree.size + 1 // one-based

        private val _children: MutableList<Node> = mutableListOf()
        override val children: List<Node> = _children

        // TODO: fix this mess
        override val modular: MultiArray<PositiveScenarioTree.Node> get() = TODO()
        // get() = this@PositiveCompoundScenarioTree.modular.map { it.nodes[id - 1] }

        init {
            parent?._children?.add(this)
            this@PositiveCompoundScenarioTree._nodes.add(this)
        }

        override fun toString(): String {
            return "Node(id=$id, parent=${parent?.id}, children=${children.map { it.id }}, element=$element)"
        }
    }
}
