package ru.ifmo.fbsat.core.scenario

import ru.ifmo.fbsat.core.utils.Compound

interface CompoundScenarioTree<S, N, T> :
    GenericScenarioTree<S, N, CompoundScenarioElement, CompoundInputAction, CompoundOutputAction>,
    Compound<T>
    where S : CompoundScenario,
          N : CompoundScenarioTree.Node,
          T : ScenarioTree<*, *> {

    interface Node : GenericScenarioTree.Node<CompoundScenarioElement, CompoundInputAction, CompoundOutputAction> {
        override val element: CompoundScenarioElement
        override val parent: Node?
        override val children: List<Node>
    }
}
