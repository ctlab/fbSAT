package ru.ifmo.fbsat.core.scenario.positive

import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.scenario.addGenericScenario
import ru.ifmo.fbsat.core.scenario.auxScenarioElement

class PositiveScenarioTree(
    override val inputEvents: List<InputEvent>,
    override val outputEvents: List<OutputEvent>,
    override val isTrie: Boolean = true
) : ScenarioTree<PositiveScenario, PositiveScenarioTree.Node> {
    private val _scenarios: MutableList<PositiveScenario> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    override val scenarios: List<PositiveScenario> = _scenarios
    override val nodes: List<Node> = _nodes

    override val size: Int get() = nodes.size
    override val root: Node get() = nodes.first()

    init {
        // Add the root
        _nodes.add(Node(element = auxScenarioElement, parent = null))
    }

    fun addScenario(scenario: PositiveScenario) {
        require(scenario.elements.isNotEmpty())

        addGenericScenario(
            scenario,
            sameNode = { _, _, child -> child },
            newNode = { _, element, current ->
                Node(element, parent = current)
            }
        )

        _scenarios.add(scenario)
    }

    inner class Node(
        override val element: ScenarioElement,
        override val parent: Node?
    ) : ScenarioTree.Node {
        private val _children: MutableList<Node> = mutableListOf()

        override val id: Int = this@PositiveScenarioTree.size + 1 // one-based
        override val children: List<Node> = _children

        init {
            parent?._children?.add(this)
            this@PositiveScenarioTree._nodes.add(this)
        }
    }
}
