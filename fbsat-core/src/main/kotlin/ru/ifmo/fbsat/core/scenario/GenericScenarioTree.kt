package ru.ifmo.fbsat.core.scenario

import ru.ifmo.fbsat.core.utils.log

interface GenericScenarioTree<S, N, out E, In, Out>
    where S : GenericScenario<E, In, Out>,
          N : GenericScenarioTree.Node<E, In, Out>,
          E : GenericScenario.Element<In, Out> {

    val isTrie: Boolean
    val scenarios: List<S>
    val nodes: List<N>
    val size: Int get() = nodes.size
    val root: N get() = nodes.first()

    interface Node<out E, In, Out> where E : GenericScenario.Element<In, Out> {
        val id: Int
        val element: E
        val parent: Node<E, In, Out>?
        val children: List<Node<E, In, Out>>
    }
}

fun <E> GenericScenarioTree.Node<E, *, *>.isSameInputAction(element: E): Boolean
    where E : GenericScenario.Element<*, *> {
    return this.element.inputAction == element.inputAction
}

internal inline fun <reified N, E> GenericScenarioTree<*, N, E, *, *>.addGenericScenario(
    scenario: GenericScenario<E, *, *>,
    noinline sameNode: (index: Int, element: E, child: N) -> N,
    noinline newNode: (index: Int, element: E, current: N) -> N
) where N : GenericScenarioTree.Node<E, *, *>,
        E : GenericScenario.Element<*, *> {
    var current = root as? N
        ?: error("Could not cast 'root' to ${N::class.java.simpleName}, what are you doing?")
    meow@ for ((index, element) in scenario.elements.withIndex()) {
        if (isTrie) {
            for (child in current.children) {
                if (child.isSameInputAction(element)) {
                    if (child.element != element) {
                        log.warn("${this::class.java.simpleName} is not deterministic!")
                        log.warn("  - current = $current")
                        log.warn("  - element = $element")
                        log.warn("  - child = $child")
                    }
                    val childF = child as? N
                        ?: error("Could not cast 'child' to ${N::class.java.simpleName}, what are you doing?")
                    current = sameNode(index, element, childF)
                    continue@meow
                }
            }
        }
        current = newNode(index, element, current)
    }
}
