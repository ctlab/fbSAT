package ru.ifmo.fbsat.core.scenario.positive

import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.scenario.addGenericScenario
import ru.ifmo.fbsat.core.scenario.auxScenarioElement
import ru.ifmo.fbsat.core.utils.MyLogger
import java.io.File

private val logger = MyLogger {}

class PositiveScenarioTree(
    override val inputEvents: List<InputEvent>,
    override val outputEvents: List<OutputEvent>,
    override val inputNames: List<String>,
    override val outputNames: List<String>,
    override val isTrie: Boolean = true,
) : ScenarioTree<PositiveScenario, PositiveScenarioTree.Node> {
    private val _scenarios: MutableList<PositiveScenario> = mutableListOf()
    private val _nodes: MutableList<Node> = mutableListOf()

    override val scenarios: List<PositiveScenario> = _scenarios
    override val nodes: List<Node> = _nodes

    override val size: Int get() = nodes.size
    override val root: Node get() = nodes.first()

    init {
        // Create the root (auto-added to _nodes)
        Node(element = auxScenarioElement, parent = null)
    }

    fun toGraphvizString(heat: Map<Int, Set<Int>> = mapOf()) =
"""digraph L {
${
    run {
        val max = heat.mapNotNull { it.value.maxOrNull() }.maxOrNull() ?: 0
        val color = heat
            .mapNotNull { it.value.minOrNull()?.let { min -> it.key to (min to it.value.size) } }
            .map {
                val (min, count) = it.second
                it.first to 100 - 100 * count / (max - min + 1)
            }
            .onEach { check(it.second in 0..100) }
            .toMap()
        nodes.joinToString("\n") {
            "qq${it.id}[label=\"${
                it.element.outputEvent?.name ?: "ε"
            }${
                it.element.outputValues.values.withIndex().filter { (_, b) -> b }.joinToString("\n", prefix = "\n") { (i, _) ->
                    outputNames[i]
                }
            }\", style=filled, fillcolor=\"grey${
                color[it.id] ?: 100
            }\", fontcolor=\"${
                if (color[it.id] ?: 100 > 51) "black" else "white"
            }\"]"
        }
    }
}
${
    nodes.mapNotNull { node ->
        node.parent?.takeIf { it.id != 1 }?.let { it to node }
    }.joinToString("\n") {
        "qq${it.first.id}->qq${it.second.id}[label=\"${
            it.second.element.inputEvent?.name ?: "ε"
        }${
            it.second.element.inputValues.values.withIndex().filter { (_, b) -> b }.joinToString("\n", prefix = "\n") { (i, _) ->
                inputNames[i]
            }
        }\"]"
    }
}
}""".trimIndent()

    fun dump(dir: File, name: String = "positive_tree", heat: Map<Int, Set<Int>> = mapOf()) {
        dir.mkdirs()
        dumpGv(dir.resolve("$name.dot"), heat)
    }

    fun dumpGv(file: File, heat: Map<Int, Set<Int>> = mapOf()) {
        file.printWriter().use {
            it.println(toGraphvizString(heat))
        }
        Runtime.getRuntime().exec("dot -Tpng -O $file")
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
        logger.info("Elements: ${scenarios.sumBy { it.elements.size }}")
        logger.info("Tree size: $size")
    }

    companion object {
        fun fromScenarios(
            scenarios: List<PositiveScenario>,
            inputNames: List<String>,
            outputNames: List<String>,
            inputEvents: List<InputEvent>? = null,
            outputEvents: List<OutputEvent>? = null,
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
            isTrie: Boolean = true,
        ): PositiveScenarioTree {
            val scenarios =
                if (file.extension == "smvout") {
                    PositiveScenario.fromSmv(
                        file = file,
                        inputEvents = inputEvents ?: listOf(InputEvent("REQ")),
                        outputEvents = outputEvents ?: listOf(OutputEvent("CNF")),
                        inputNames = inputNames,
                        outputNames = outputNames
                    )
                } else {
                    PositiveScenario.fromFile(file)
                }
            return fromScenarios(
                scenarios = scenarios,
                inputNames = inputNames,
                outputNames = outputNames,
                inputEvents = inputEvents,
                outputEvents = outputEvents,
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
