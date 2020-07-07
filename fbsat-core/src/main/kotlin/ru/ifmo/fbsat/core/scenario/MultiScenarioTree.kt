package ru.ifmo.fbsat.core.scenario

import com.github.lipen.lazycache.LazyCache
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.automaton.OutputValues
import ru.ifmo.fbsat.core.utils.Globals

val auxElement: ScenarioElement = ScenarioElement(
    InputAction(
        event = null,
        values = InputValues.empty()
    ),
    OutputAction(
        event = null,
        values = Globals.INITIAL_OUTPUT_VALUES
    )
)

@Suppress("MemberVisibilityCanBePrivate")
class MultiScenarioTree(
    val numberOfModules: Int,
    val modularInputEvents: MultiArray<List<InputEvent>>,
    val modularOutputEvents: MultiArray<List<OutputEvent>>,
    val modularInputNames: MultiArray<List<String>>,
    val modularOutputNames: MultiArray<List<String>>,
    private val isTrue: Boolean = true
) {
    private val lazyCache = LazyCache()
    private val _scenarios: MutableList<PositiveMultiScenario> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    val M: Int = numberOfModules
    val scenarios: List<PositiveMultiScenario> = _scenarios
    val nodes: List<Node> = _nodes
    val root: Node

    val size: Int by lazyCache {
        nodes.size
    }

    init {
        root = Node(
            multiElement = MultiArray.create(M) { auxElement },
            parent = null
        )
        _nodes.add(root)
        lazyCache.invalidate()
    }

    inner class Node(
        val multiElement: MultiScenarioElement,
        val parent: Node?
    ) {
        private val _children: MutableList<Node> = mutableListOf()

        val id: Int = this@MultiScenarioTree.size + 1 // Note: one-based
        val children: List<Node> = _children

        val modularInputAction: MultiArray<InputAction> = multiElement.modularInputAction
        val modularOutputAction: MultiArray<OutputAction> = multiElement.modularOutputAction
        val modularInputEvent: MultiArray<InputEvent?> = multiElement.modularInputEvent
        val modularInputValues: MultiArray<InputValues> = multiElement.modularInputValues
        val modularOutputEvent: MultiArray<OutputEvent?> = multiElement.modularOutputEvent
        val modularOutputValues: MultiArray<OutputValues> = multiElement.modularOutputValues

        init {
            parent?._children?.add(this)
            this@MultiScenarioTree._nodes.add(this)
            this@MultiScenarioTree.lazyCache.invalidate()
        }

        override fun toString(): String {
            return "Node(id=$id, element=$multiElement, parent=${parent?.id}, children=${children.map { it.id }})"
        }
    }

    fun addMultiScenario(multiScenario: PositiveMultiScenario) {
        var current = root

        meow@ for (element in multiScenario.elements) {
            if (isTrue) {
                // TODO: trie logic for MultiScenarioTree
                for (child in current.children) {
                    if (child.modularInputAction == element.modularInputAction) {
                        check(child.multiElement == element) {
                            println("current = $current")
                            println("element = $element")
                            println("child = $child")
                            "ScenarioTree is not deterministic!"
                        }
                        current = child
                        // TODO: element.nodeId = current.id
                        continue@meow
                    }
                }
            }
            current = Node(element, current)
            // TODO: element.nodeId = current.id
        }

        _scenarios.add(multiScenario)
        lazyCache.invalidate()
    }
}
