package ru.ifmo.fbsat.core.scenario

import ru.ifmo.fbsat.core.utils.log

interface GenericScenarioTree<S, N>
    where S : GenericScenario<GenericScenario.Element<*, *>>,
          N : GenericScenarioTree.Node<GenericScenario.Element<*, *>> {

    val isTrie: Boolean
    val scenarios: List<S>
    val nodes: List<N>
    val size: Int get() = nodes.size
    val root: N get() = nodes.first()

    interface Node<out E> where E : GenericScenario.Element<*, *> {
        val id: Int
        val element: E
        val parent: Node<E>?
        val children: List<Node<E>>
    }
}

// TODO: this function can be inlined
fun <E> GenericScenarioTree.Node<E>.isSameInputAction(element: E): Boolean
    where E : GenericScenario.Element<*, *> {
    return this.element.inputAction == element.inputAction
}

internal inline fun <S, reified N, E> GenericScenarioTree<S, N>.addGenericScenario(
    scenario: S,
    noinline sameNode: (index: Int, element: E, child: N) -> N,
    noinline newNode: (index: Int, element: E, current: N) -> N
) where S : GenericScenario<E>,
        N : GenericScenarioTree.Node<E>,
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
                    val childN = child as? N
                        ?: error("Could not cast 'child' to ${N::class.java.simpleName}, what are you doing?")
                    current = sameNode(index, element, childN)
                    continue@meow
                }
            }
        }
        current = newNode(index, element, current)
    }
}
