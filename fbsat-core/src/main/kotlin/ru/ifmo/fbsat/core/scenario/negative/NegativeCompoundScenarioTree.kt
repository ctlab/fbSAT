package ru.ifmo.fbsat.core.scenario.negative

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.scenario.CompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.scenario.addGenericScenario
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.project

// TODO: copy changes from PositiveCompoundScenarioTree
class NegativeCompoundScenarioTree(
    override val M: Int,
    override val modularInputEvents: MultiArray<List<InputEvent>>,
    override val modularOutputEvents: MultiArray<List<OutputEvent>>,
    override val modularInputNames: MultiArray<List<String>>,
    override val modularOutputNames: MultiArray<List<String>>,
    override val isTrie: Boolean = false,
) : CompoundScenarioTree<NegativeScenarioTree, NegativeCompoundScenario, NegativeCompoundScenarioTree.Node>,
    CompoundImpl<NegativeScenarioTree>() {

    init {
        // require(modularInputEvents.shape.single() == M)
        // require(modularOutputEvents.shape.single() == M)
        // require(modularInputNames.shape.single() == M)
        // require(modularOutputNames.shape.single() == M)
    }

    private val _scenarios: MutableList<NegativeCompoundScenario> = mutableListOf()
    override val scenarios: List<NegativeCompoundScenario> = _scenarios

    private val _nodes: MutableList<Node> = mutableListOf()
    override val nodes: List<Node> = _nodes

    override val size: Int get() = nodes.size
    override val root: Node get() = nodes.first()

    override val modular: MultiArray<NegativeScenarioTree> =
        MultiArray.new(M) { (m) ->
            NegativeScenarioTree(
                inputEvents = modularInputEvents[m],
                outputEvents = modularOutputEvents[m],
                inputNames = modularInputNames[m],
                outputNames = modularOutputNames[m],
                isTrie = isTrie
            )
        }

    init {
        // Create the root (auto-added to _nodes)
        Node(
            element = CompoundScenarioElement(
                MultiArray.new(M) { (m) ->
                    ScenarioElement(
                        InputAction(
                            event = null,
                            values = InputValues.empty()
                        ),
                        OutputAction(
                            event = null,
                            values = OutputValues.zeros(modularOutputNames[m].size) // FIXME
                        )
                    )
                }
            ),
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
                        // log.debug { "[${index + 1}/${scenario.elements.size}] (compound) [same] loopBack now = $loopBack" }
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
                        // log.debug { "[${index + 1}/${scenario.elements.size}] (compound) [new] loopBack now = $loopBack" }
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

        // println("Added negative compound scenario")
        _scenarios.add(scenario)
        for (m in 1..M) {
            // println("Also adding the projected compound scenario to module m = $m")
            project(m).addScenario(scenario.project(m))
            // println("done")
        }
    }

    inner class Node(
        override val element: CompoundScenarioElement,
        override val parent: Node?,
    ) : CompoundScenarioTree.Node<Node>, CompoundImpl<ScenarioTree.Node<*>>() {
        override val M: Int = this@NegativeCompoundScenarioTree.M
        override val id: Int = this@NegativeCompoundScenarioTree.size + 1 // one-based

        private val _children: MutableList<Node> = mutableListOf()
        override val children: List<Node> = _children

        val loopBacks: MutableSet<Node> = mutableSetOf()
        internal var isLoopBack: Boolean = false

        // TODO: fix this mess
        override val modular: MultiArray<NegativeScenarioTree.Node> get() = TODO()
        // get() = this@NegativeCompoundScenarioTree.modular.map { it.nodes[id - 1] }

        init {
            parent?._children?.add(this)
            this@NegativeCompoundScenarioTree._nodes.add(this)
        }

        override fun toString(): String {
            return "Node(id=$id, parent=${parent?.id}, children=${children.map { it.id }}, element=$element)"
        }
    }
}
