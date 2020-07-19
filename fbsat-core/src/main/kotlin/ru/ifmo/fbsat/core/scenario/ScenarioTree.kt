package ru.ifmo.fbsat.core.scenario

interface ScenarioTree<S, N> : GenericScenarioTree<S, N>
    where S : Scenario,
          N : ScenarioTree.Node {

    val inputEvents: List<InputEvent>
    val outputEvents: List<OutputEvent>
    val inputNames: List<String>
    val outputNames: List<String>
    val uniqueInputs: List<InputValues>
    val uniqueOutputs: List<OutputValues>

    interface Node : GenericScenarioTree.Node<ScenarioElement>
}

fun ScenarioTree<*, *>.parent(v: Int): Int =
    nodes[v - 1].parent?.id ?: 0

fun ScenarioTree<*, *>.inputEvent(v: Int): Int =
    inputEvents.indexOf(nodes[v - 1].element.inputEvent) + 1

fun ScenarioTree<*, *>.outputEvent(v: Int): Int =
    outputEvents.indexOf(nodes[v - 1].element.outputEvent) + 1

fun ScenarioTree<*, *>.inputNumber(v: Int): Int =
    uniqueInputs.indexOf(nodes[v - 1].element.inputValues) + 1

fun ScenarioTree<*, *>.outputNumber(v: Int): Int =
    uniqueOutputs.indexOf(nodes[v - 1].element.outputValues) + 1

fun ScenarioTree<*, *>.inputValue(v: Int, x: Int): Boolean =
    nodes[v - 1].element.inputValues[x - 1]

fun <N : ScenarioTree.Node> ScenarioTree<*, N>.outputValue(v: Int, z: Int): Boolean =
    nodes[v - 1].element.outputValues[z - 1]
