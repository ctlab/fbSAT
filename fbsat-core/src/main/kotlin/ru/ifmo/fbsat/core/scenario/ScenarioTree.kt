package ru.ifmo.fbsat.core.scenario

interface ScenarioTree<S, N> : GenericScenarioTree<S, N>
    where S : Scenario,
          N : ScenarioTree.Node<*> {

    interface Node<out Self : Node<Self>> : GenericScenarioTree.Node<Self, ScenarioElement>

    val inputEvents: List<InputEvent>
    val outputEvents: List<OutputEvent>
    val inputNames: List<String>
    val outputNames: List<String>
    val uniqueInputs: List<InputValues>
    val uniqueOutputs: List<OutputValues>

    fun parent(v: Int): Int =
        nodes[v - 1].parent?.id ?: 0

    fun inputEvent(v: Int): Int =
        inputEvents.indexOf(nodes[v - 1].element.inputEvent) + 1

    fun outputEvent(v: Int): Int =
        outputEvents.indexOf(nodes[v - 1].element.outputEvent) + 1

    fun inputNumber(v: Int): Int =
        uniqueInputs.indexOf(nodes[v - 1].element.inputValues) + 1

    fun outputNumber(v: Int): Int =
        uniqueOutputs.indexOf(nodes[v - 1].element.outputValues) + 1

    fun inputValue(v: Int, x: Int): Boolean =
        nodes[v - 1].element.inputValues[x - 1]

    fun outputValue(v: Int, z: Int): Boolean =
        nodes[v - 1].element.outputValues[z - 1]
}

@Deprecated("Use member instead.", ReplaceWith("parent"), DeprecationLevel.ERROR)
fun ScenarioTree<*, *>.parent(v: Int): Int =
    nodes[v - 1].parent?.id ?: 0

@Deprecated("Use member instead.", ReplaceWith("inputEvent"), DeprecationLevel.ERROR)
fun ScenarioTree<*, *>.inputEvent(v: Int): Int =
    inputEvents.indexOf(nodes[v - 1].element.inputEvent) + 1

@Deprecated("Use member instead.", ReplaceWith("outputEvent"), DeprecationLevel.ERROR)
fun ScenarioTree<*, *>.outputEvent(v: Int): Int =
    outputEvents.indexOf(nodes[v - 1].element.outputEvent) + 1

@Deprecated("Use member instead.", ReplaceWith("inputNumber"), DeprecationLevel.ERROR)
fun ScenarioTree<*, *>.inputNumber(v: Int): Int =
    uniqueInputs.indexOf(nodes[v - 1].element.inputValues) + 1

@Deprecated("Use member instead.", ReplaceWith("outputNumber"), DeprecationLevel.ERROR)
fun ScenarioTree<*, *>.outputNumber(v: Int): Int =
    uniqueOutputs.indexOf(nodes[v - 1].element.outputValues) + 1

@Deprecated("Use member instead.", ReplaceWith("inputValue"), DeprecationLevel.ERROR)
fun ScenarioTree<*, *>.inputValue(v: Int, x: Int): Boolean =
    nodes[v - 1].element.inputValues[x - 1]

@Deprecated("Use member instead.", ReplaceWith("outputValue"), DeprecationLevel.ERROR)
fun ScenarioTree<*, *>.outputValue(v: Int, z: Int): Boolean =
    nodes[v - 1].element.outputValues[z - 1]
