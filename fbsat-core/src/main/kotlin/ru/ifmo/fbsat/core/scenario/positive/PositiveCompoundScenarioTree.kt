package ru.ifmo.fbsat.core.scenario.positive

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.scenario.CompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.addGenericScenario
import ru.ifmo.fbsat.core.scenario.auxScenarioElement
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.M
import ru.ifmo.fbsat.core.utils.toImmutable

class PositiveCompoundScenarioTree(
    val numberOfModules: Int,
    val modularInputEvents: MultiArray<List<InputEvent>>,
    val modularOutputEvents: MultiArray<List<OutputEvent>>,
    val modularInputNames: MultiArray<List<String>>,
    val modularOutputNames: MultiArray<List<String>>,
    override val isTrie: Boolean = true
) : CompoundScenarioTree<PositiveCompoundScenario,
    PositiveCompoundScenarioTree.Node, PositiveScenarioTree> {

    init {
        require(modularInputEvents.shape.single() == numberOfModules)
        require(modularOutputEvents.shape.single() == numberOfModules)
        require(modularInputNames.shape.single() == numberOfModules)
        require(modularOutputNames.shape.single() == numberOfModules)
    }

    private val _scenarios: MutableList<PositiveCompoundScenario> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    override val scenarios: List<PositiveCompoundScenario> = _scenarios
    override val nodes: List<Node> = _nodes

    override val size: Int get() = nodes.size
    override val root: Node get() = nodes.first()

    override val modular: ImmutableMultiArray<PositiveScenarioTree>
        get() = MultiArray.create(numberOfModules) { (m) ->
            PositiveScenarioTree(
                inputEvents = modularInputEvents[m],
                outputEvents = modularOutputEvents[m],
                inputNames = modularInputNames[m],
                outputNames = modularOutputNames[m],
                isTrie = isTrie
            ).also {
                for (scenario in scenarios) {
                    it.addScenario(scenario.modular[m])
                }
            }
        }.toImmutable()

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
