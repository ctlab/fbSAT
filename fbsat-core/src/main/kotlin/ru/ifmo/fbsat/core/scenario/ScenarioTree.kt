package ru.ifmo.fbsat.core.scenario

interface ScenarioTree<S, N> : GenericScenarioTree<S, N, ScenarioElement, InputAction, OutputAction>
    where S : Scenario,
          N : ScenarioTree.Node {

    val inputEvents: List<InputEvent>
    val outputEvents: List<OutputEvent>

    interface Node : GenericScenarioTree.Node<ScenarioElement, InputAction, OutputAction> {
        override val element: ScenarioElement
        override val parent: Node?
        override val children: List<Node>
    }
}
