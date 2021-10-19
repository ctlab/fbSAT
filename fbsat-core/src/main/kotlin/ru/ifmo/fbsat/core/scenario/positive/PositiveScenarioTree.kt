package ru.ifmo.fbsat.core.scenario.positive

import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.scenario.addGenericScenario
import ru.ifmo.fbsat.core.scenario.initialOutputValues
import ru.ifmo.fbsat.core.utils.MyLogger
import java.io.File

private val logger = MyLogger {}

class PositiveScenarioTree(
    override val inputEvents: List<InputEvent>,
    override val outputEvents: List<OutputEvent>,
    override val inputNames: List<String>,
    override val outputNames: List<String>,
    initialOutputValues: OutputValues? = null,
    override val isTrie: Boolean = true,
) : ScenarioTree<PositiveScenario, PositiveScenarioTree.Node> {
    private val _scenarios: MutableList<PositiveScenario> = mutableListOf()
    override val scenarios: List<PositiveScenario> = _scenarios

    private val _nodes: MutableList<Node> = mutableListOf()
    override val nodes: List<Node> = _nodes

    init {
        // Create the root (auto-added to _nodes)
        Node(
            element = ScenarioElement(
                InputAction(
                    event = null,
                    values = InputValues.empty()
                ),
                OutputAction(
                    event = null,
                    values = initialOutputValues ?: OutputValues.zeros(outputNames.size)
                )
            ),
            parent = null
        )
    }

    fun addScenario(scenario: PositiveScenario) {
        require(scenario.elements.isNotEmpty())

        addGenericScenario(
            scenario,
            sameNode = { _, _, child -> child },
            newNode = { _, element, current ->
                Node(element, parent = current)
            }
        )

        _scenarios.add(scenario)
    }

    inner class Node(
        override val element: ScenarioElement,
        override val parent: Node?,
    ) : ScenarioTree.Node<Node> {
        private val _children: MutableList<Node> = mutableListOf()

        override val id: Int = this@PositiveScenarioTree.size + 1 // one-based
        override val children: List<Node> = _children

        init {
            parent?._children?.add(this)
            this@PositiveScenarioTree._nodes.add(this)
        }

        override fun toString(): String {
            return "Node(id=$id, parent=${parent?.id}, children=${children.map { it.id }}, element=$element)"
        }
    }

    fun printStats() {
        logger.info("Scenarios: ${scenarios.size}")
        logger.info("Elements: ${scenarios.sumOf({ it.elements.size })}")
        logger.info("Tree size: $size")
        logger.info("Initial output values: $initialOutputValues")
        logger.info("Unique inputs: ${uniqueInputs.size}")
        logger.info("Unique outputs: ${uniqueOutputs.size}")
    }

    companion object {
        fun fromScenarios(
            scenarios: List<PositiveScenario>,
            inputNames: List<String>,
            outputNames: List<String>,
            inputEvents: List<InputEvent>? = null,
            outputEvents: List<OutputEvent>? = null,
            initialOutputValues: OutputValues? = null,
            isTrie: Boolean = true,
        ): PositiveScenarioTree =
            PositiveScenarioTree(
                inputEvents = inputEvents ?: scenarios.flatMap { scenario ->
                    scenario.elements.mapNotNull { element -> element.inputAction.event }
                }.distinct(),
                outputEvents = outputEvents ?: scenarios.flatMap { scenario ->
                    scenario.elements.mapNotNull { element -> element.outputAction.event }
                }.distinct(),
                inputNames = inputNames,
                outputNames = outputNames,
                initialOutputValues = initialOutputValues,
                isTrie = isTrie
            ).also {
                for (scenario in scenarios) {
                    it.addScenario(scenario)
                }
            }

        fun fromFile(
            file: File,
            inputNames: List<String>,
            outputNames: List<String>,
            inputEvents: List<InputEvent>? = null,
            outputEvents: List<OutputEvent>? = null,
            initialOutputValues: OutputValues? = null,
            isTrie: Boolean = true,
        ): PositiveScenarioTree {
            val scenarios =
                when (file.extension) {
                    "json" -> {
                        PositiveScenario.fromJson(file)
                    }
                    "smvout" -> {
                        PositiveScenario.fromSmvout(
                            file = file,
                            inputEvents = inputEvents ?: listOf(InputEvent("REQ")),
                            outputEvents = outputEvents ?: listOf(OutputEvent("CNF")),
                            inputNames = inputNames,
                            outputNames = outputNames
                        )
                    }
                    else -> {
                        PositiveScenario.fromFile(
                            file = file,
                            initialOutputValues = initialOutputValues ?: OutputValues.zeros(outputNames.size)
                        )
                    }
                }
            return fromScenarios(
                scenarios = scenarios,
                inputNames = inputNames,
                outputNames = outputNames,
                inputEvents = inputEvents,
                outputEvents = outputEvents,
                initialOutputValues = initialOutputValues,
                isTrie = isTrie
            )
        }

        fun fromOld(oldTree: OldPositiveScenarioTree): PositiveScenarioTree =
            PositiveScenarioTree(
                inputEvents = oldTree.inputEvents,
                outputEvents = oldTree.outputEvents,
                inputNames = oldTree.inputNames,
                outputNames = oldTree.outputNames
            ).also {
                for (scenario in oldTree.scenarios) {
                    it.addScenario(scenario)
                }
            }
    }
}

fun PositiveScenarioTree.toOld(): OldPositiveScenarioTree {
    return OldPositiveScenarioTree(
        inputEvents = inputEvents,
        outputEvents = outputEvents,
        inputNames = inputNames,
        outputNames = outputNames,
        isTrie = isTrie
    ).also {
        for (scenario in scenarios) {
            it.addScenario(scenario)
        }
    }
}
