package ru.ifmo.fbsat.core.scenario

import com.github.lipen.lazycache.LazyCache
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.automaton.OutputValues
import ru.ifmo.fbsat.core.utils.toImmutable

// TODO: convert to `NegativeCompoundScenarioTree`
@Suppress("MemberVisibilityCanBePrivate", "PropertyName")
class CompoundScenarioTree(
    val numberOfModules: Int,
    val modularInputEvents: MultiArray<List<InputEvent>>,
    val modularOutputEvents: MultiArray<List<OutputEvent>>,
    val modularInputNames: MultiArray<List<String>>,
    val modularOutputNames: MultiArray<List<String>>,
    private val isTrue: Boolean = true
) {
    private val lazyCache = LazyCache()
    private val _scenarios: MutableList<PositiveCompoundScenario> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    val M: Int = numberOfModules
    val scenarios: List<PositiveCompoundScenario> = _scenarios
    val nodes: List<Node> = _nodes
    val root: Node

    val size: Int by lazyCache {
        nodes.size
    }

    init {
        root = Node(
            compoundElement = CompoundScenarioElement(MultiArray.create(M) { auxScenarioElement }.toImmutable()),
            parent = null
        )
        _nodes.add(root)
        lazyCache.invalidate()
    }

    inner class Node(
        val compoundElement: CompoundScenarioElement,
        val parent: Node?
    ) {
        private val _children: MutableList<Node> = mutableListOf()

        val id: Int = this@CompoundScenarioTree.size + 1 // Note: one-based
        val children: List<Node> = _children

        val modularInputAction: MultiArray<InputAction> = compoundElement.modularInputAction
        val modularOutputAction: MultiArray<OutputAction> = compoundElement.modularOutputAction
        val modularInputEvent: MultiArray<InputEvent?> = compoundElement.modularInputEvent
        val modularInputValues: MultiArray<InputValues> = compoundElement.modularInputValues
        val modularOutputEvent: MultiArray<OutputEvent?> = compoundElement.modularOutputEvent
        val modularOutputValues: MultiArray<OutputValues> = compoundElement.modularOutputValues

        init {
            parent?._children?.add(this)
            this@CompoundScenarioTree._nodes.add(this)
            this@CompoundScenarioTree.lazyCache.invalidate()
        }

        override fun toString(): String {
            return "Node(id=$id, element=$compoundElement, parent=${parent?.id}, children=${children.map { it.id }})"
        }
    }

    fun addCompoundScenario(compoundScenario: PositiveCompoundScenario) {
        var current = root

        meow@ for (element in compoundScenario.elements) {
            if (isTrue) {
                for (child in current.children) {
                    if (child.modularInputAction == element.modularInputAction) {
                        check(child.compoundElement == element) {
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

        _scenarios.add(compoundScenario)
        lazyCache.invalidate()
    }
}
