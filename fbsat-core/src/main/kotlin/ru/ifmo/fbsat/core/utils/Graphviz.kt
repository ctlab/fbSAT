@file:Suppress("MemberVisibilityCanBePrivate")

package ru.ifmo.fbsat.core.utils

class Graph(val name: String) {
    private val _graphSetup: MutableMap<String, String> = mutableMapOf()
    private val _nodeSetup: MutableMap<String, String> = mutableMapOf()
    private val _edgeSetup: MutableMap<String, String> = mutableMapOf()
    private val _nodes: MutableMap<Int, Node> = mutableMapOf()
    private val _edges: MutableList<Edge> = mutableListOf()

    val graphSetup: Map<String, String> = _graphSetup
    val nodeSetup: Map<String, String> = _nodeSetup
    val edgeSetup: Map<String, String> = _edgeSetup

    val nodes: List<Node>
        get() = _nodes.values.toList()

    val edges: List<Edge> = _edges

    fun graphSetup(vararg attributes: Pair<String, String>) {
        _graphSetup.putAll(attributes)
    }

    fun nodeSetup(vararg attributes: Pair<String, String>) {
        _nodeSetup.putAll(attributes)
    }

    fun edgeSetup(vararg attributes: Pair<String, String>) {
        _edgeSetup.putAll(attributes)
    }

    fun setup(vararg attributes: Pair<String, String>) {
        graphSetup(*attributes)
        nodeSetup(*attributes)
        edgeSetup(*attributes)
    }

    private fun getNode(id: Int) = _nodes.computeIfAbsent(id) { Node(id = it) }

    fun node(id: Int, vararg attributes: Pair<String, String>): Node {
        return getNode(id).also { it.addAttributes(attributes) }
    }

    fun edge(src: Int, dest: Int, vararg attributes: Pair<String, String>): Edge {
        return Edge(source = getNode(src), destination = getNode(dest))
            .also { it.addAttributes(attributes) }
            .also { _edges.add(it) }
    }

    fun toGraphvisString(): String {
        fun Map<String, String>.setupString(): String {
            return asSequence().joinToString(" ") { (name, value) -> "$name=\"$value\"" }
        }

        val lines: MutableList<String> = mutableListOf()

        if (graphSetup.isNotEmpty())
            lines += "graph [${graphSetup.setupString()}]"
        if (nodeSetup.isNotEmpty())
            lines += "node [${nodeSetup.setupString()}]"
        if (edgeSetup.isNotEmpty())
            lines += "edge [${edgeSetup.setupString()}]"

        lines += "// Nodes"
        lines.addAll(nodes.asSequence().map(Node::toGraphvizString))

        lines += "// Edges"
        lines.addAll(edges.asSequence().map(Edge::toGraphvizString))

        return "digraph${if (name.isNotBlank()) " $name" else ""} {\n${lines.joinToString("\n") { if (it.isBlank()) "" else "    $it" }}\n}"
    }

    override fun toString(): String {
        return "Graph(name = $name)"
    }

    data class Node(
        val id: Int
    ) {
        private val _attributes: MutableMap<String, String> = mutableMapOf()

        val attributes: Map<String, String> = _attributes

        fun addAttributes(attributes: Array<out Pair<String, String>>) {
            _attributes.putAll(attributes)
        }

        @JvmName("addAttributesVararg")
        fun addAttributes(vararg attributes: Pair<String, String>) {
            addAttributes(attributes)
        }

        operator fun invoke(vararg attributes: Pair<String, String>) {
            addAttributes(attributes)
        }

        fun toGraphvizString(): String {
            var s = "$id"
            if (attributes.isNotEmpty())
                s += " [${
                    attributes.asSequence().joinToString(" ") { (name, value) ->
                        // do not enquote html-like labels
                        if (name == "label" && value.startsWith("<") && value.endsWith(">"))
                            "$name=$value"
                        else
                            "$name=\"$value\""
                    }
                }]"
            return s
        }
    }

    data class Edge(
        val source: Node,
        val destination: Node
    ) {
        private val _attributes: MutableMap<String, String> = mutableMapOf()

        val attributes: Map<String, String> = _attributes

        fun addAttributes(attributes: Array<out Pair<String, String>>) {
            _attributes.putAll(attributes)
        }

        @JvmName("addAttributesVararg")
        fun addAttributes(vararg attributes: Pair<String, String>) {
            addAttributes(attributes)
        }

        operator fun invoke(vararg attributes: Pair<String, String>) {
            addAttributes(attributes)
        }

        fun toGraphvizString(): String {
            var s = "${source.id} -> ${destination.id}"
            if (attributes.isNotEmpty())
                s += " [${attributes.asSequence().joinToString(" ") { (name, value) -> "$name=\"$value\"" }}]"
            return s
        }
    }
}

fun graph(name: String = "", init: Graph.() -> Unit): Graph {
    return Graph(name).also(init)
}
