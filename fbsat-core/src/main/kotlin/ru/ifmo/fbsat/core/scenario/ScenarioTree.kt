package ru.ifmo.fbsat.core.scenario

interface ScenarioTree<S, N> : GenericScenarioTree<S, N>
    where S : Scenario,
          N : ScenarioTree.Node {

    val inputEvents: List<InputEvent>
    val outputEvents: List<OutputEvent>
    val inputNames: List<String>
    val outputNames: List<String>

    interface Node : GenericScenarioTree.Node<ScenarioElement>
}
