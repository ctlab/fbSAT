package ru.ifmo.fbsat.core.scenario.positive

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.scenario.CompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.addGenericScenario
import ru.ifmo.fbsat.core.scenario.auxScenarioElement
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.M

class PositiveCompoundScenarioTree(
    override val modular: ImmutableMultiArray<PositiveScenarioTree>,
    override val isTrie: Boolean = true
) : CompoundScenarioTree<PositiveCompoundScenario, PositiveCompoundScenarioTree.Node, PositiveScenarioTree> {

    private val _scenarios: MutableList<PositiveCompoundScenario> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    override val scenarios: List<PositiveCompoundScenario> = _scenarios
    override val nodes: List<Node> = _nodes

    override val size: Int get() = nodes.size
    override val root: Node get() = nodes.first()

    init {
        // Add the root
        _nodes.add(
            Node(
                element = CompoundScenarioElement(MultiArray.create(M) { auxScenarioElement }),
                parent = null
            )
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
        _scenarios.add(scenario)
    }

    inner class Node(
        override val element: CompoundScenarioElement,
        override val parent: Node?
    ) : CompoundScenarioTree.Node {
        private val _children: MutableList<Node> = mutableListOf()

        override val id: Int = this@PositiveCompoundScenarioTree.size + 1 // one-based
        override val children: List<Node> = _children

        init {
            parent?._children?.add(this)
            this@PositiveCompoundScenarioTree._nodes.add(this)
        }
    }
}
