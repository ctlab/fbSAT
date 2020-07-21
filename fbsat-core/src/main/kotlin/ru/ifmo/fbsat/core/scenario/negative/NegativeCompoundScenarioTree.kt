package ru.ifmo.fbsat.core.scenario.negative

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.scenario.CompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.scenario.addGenericScenario
import ru.ifmo.fbsat.core.scenario.auxScenarioElement
import ru.ifmo.fbsat.core.utils.Compound
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.project
import ru.ifmo.fbsat.core.utils.toImmutable

// TODO: copy changes from PositiveCompoundScenarioTree
class NegativeCompoundScenarioTree(
    override val M: Int,
    override val modularInputEvents: MultiArray<List<InputEvent>>,
    override val modularOutputEvents: MultiArray<List<OutputEvent>>,
    override val modularInputNames: MultiArray<List<String>>,
    override val modularOutputNames: MultiArray<List<String>>,
    override val isTrie: Boolean = true
) : CompoundScenarioTree<NegativeCompoundScenario, NegativeCompoundScenarioTree.Node>,
    CompoundImpl<ScenarioTree<*, *>>() {

    init {
        require(modularInputEvents.shape.single() == M)
        require(modularOutputEvents.shape.single() == M)
        require(modularInputNames.shape.single() == M)
        require(modularOutputNames.shape.single() == M)
    }

    override val modular: ImmutableMultiArray<NegativeScenarioTree>
        get() = MultiArray.create(M) { (m) ->
            NegativeScenarioTree(
                inputEvents = modularInputEvents[m],
                outputEvents = modularOutputEvents[m],
                inputNames = modularInputNames[m],
                outputNames = modularOutputNames[m],
                isTrie = isTrie
            ).also {
                for (scenario in scenarios) {
                    it.addScenario(scenario.project(m))
                }
            }
        }.toImmutable()

    private val _scenarios: MutableList<NegativeCompoundScenario> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    override val scenarios: List<NegativeCompoundScenario> = _scenarios
    override val nodes: List<Node> = _nodes

    override val size: Int get() = nodes.size
    override val root: Node get() = nodes.first()

    init {
        // Create the root (auto-added to _nodes)
        Node(
            element = CompoundScenarioElement(MultiArray.create(M) { auxScenarioElement }),
            parent = null
        )
    }

    fun loopBacks(v: Int): List<Int> = nodes[v - 1].loopBacks.map { it.id }

    @Suppress("DuplicatedCode")
    fun addScenario(scenario: NegativeCompoundScenario) {
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
        for (m in 1..M) {
            modular[m].addScenario(scenario.project(m))
        }
    }

    inner class Node(
        override val element: CompoundScenarioElement,
        override val parent: Node?
    ) : CompoundScenarioTree.Node<Node>, CompoundImpl<ScenarioTree.Node<*>>() {
        override val M: Int = this@NegativeCompoundScenarioTree.M
        override val id: Int = this@NegativeCompoundScenarioTree.size + 1 // one-based

        private val _children: MutableList<Node> = mutableListOf()
        override val children: List<Node> = _children

        val loopBacks: MutableSet<Node> = mutableSetOf()
        internal var isLoopBack: Boolean = false

        // TODO: fix this mess
        override val modular: ImmutableMultiArray<NegativeScenarioTree.Node> get() = TODO()
        // get() = this@NegativeCompoundScenarioTree.modular.map { it.nodes[id - 1] }.toImmutable()

        init {
            parent?._children?.add(this)
            this@NegativeCompoundScenarioTree._nodes.add(this)
        }

        override fun toString(): String {
            return "Node(id=$id, parent=${parent?.id}, children=${children.map { it.id }}, element=$element)"
        }
    }
}
