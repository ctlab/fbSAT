package ru.ifmo.fbsat.core.scenario

import ru.ifmo.fbsat.core.utils.mylog

interface GenericScenarioTree<S, N>
    where S : GenericScenario<*>,
          N : GenericScenarioTree.Node<*, *> {

    val isTrie: Boolean
    val scenarios: List<S>
    val nodes: List<N>
    val size: Int get() = nodes.size
    val root: N get() = nodes.first()

    interface Node<out Self, out E>
        where Self : Node<Self, E>,
              E : GenericScenario.Element<*, *> {
        val id: Int
        val element: E
        val parent: Self?
        val children: List<Self>
    }
}

internal inline fun <N, E> GenericScenarioTree<*, N>.addGenericScenario(
    scenario: GenericScenario<E>,
    sameNode: (index: Int, element: E, child: N) -> N,
    newNode: (index: Int, element: E, current: N) -> N,
) where N : GenericScenarioTree.Node<N, E>,
        E : GenericScenario.Element<*, *> {
    var current = root
    meow@ for ((index, element) in scenario.elements.withIndex()) {
        if (isTrie) {
            for (child in current.children) {
                if (child.element.inputAction == element.inputAction) {
                    if (child.element != element) {
                        mylog.warn("${this::class.java.simpleName} is not deterministic!")
                        mylog.warn("  - current = $current")
                        mylog.warn("  - element = $element")
                        mylog.warn("  - child = $child")
                    }
                    current = sameNode(index, element, child)
                    element.nodeId = current.id
                    continue@meow
                }
            }
        }
        current = newNode(index, element, current)
        element.nodeId = current.id
    }
}
