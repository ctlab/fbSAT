package ru.ifmo.fbsat.automaton

import ru.ifmo.fbsat.utils.IntMultiArray
import ru.ifmo.fbsat.utils.MultiArray
import ru.ifmo.fbsat.utils.toBooleanString

interface Guard {
    val size: Int
    fun eval(inputValues: BooleanArray): Boolean
    fun toSimpleString(): String
    fun toGraphvizString(): String
    fun toSmvString(): String
}

class TruthTableGuard(
    val truthTable: String,
    private val inputNames: List<String>,
    private val uniqueInputs: List<String>
) : Guard {
    override val size: Int
        get() {
            println("[!] TruthTableGuard has no meaningful size")
            return 0
        }

    init {
        require(truthTable.length == uniqueInputs.size) { "Truth table size and number of unique inputs mismatch" }
    }

    override fun eval(inputValues: BooleanArray): Boolean {
        return truthTable[uniqueInputs.indexOf(inputValues.toBooleanString())] in "1x"
    }

    override fun toSimpleString(): String {
        return "[$truthTable]"
    }

    override fun toGraphvizString(): String =
        if (truthTable.length <= 8)
            "[$truthTable]"
        else
            "[${truthTable.substring(0, 3)}${Typography.ellipsis}]"

    override fun toSmvString(): String {
        return truthTable
            .asIterable()
            .zip(uniqueInputs)
            // .filter { it.first == '1' }
            .filter { it.first in "1x" }
            .joinToString(" | ", prefix = "(", postfix = ")") { (_, input) ->
                input
                    .asIterable()
                    .zip(inputNames)
                    .joinToString("&") { (value, name) ->
                        when (value) {
                            '1' -> name
                            '0' -> "!$name"
                            else -> throw Exception("...")
                        }
                    }
            }
    }

    override fun toString(): String {
        return "TruthTableGuard(tt = $truthTable)"
    }
}

class ParseTreeGuard(
    nodeType: MultiArray<NodeType>,
    terminal: IntMultiArray,
    parent: IntMultiArray,
    childLeft: IntMultiArray,
    childRight: IntMultiArray,
    private val inputNames: List<String>? = null
) : Guard {
    val nodes: List<Node>

    val root: Node
        get() = nodes.first()

    override val size: Int
        get() = root.size

    init {
        val (P) = nodeType.shape
        this.nodes = List(P) { p0 -> Node(nodeType[p0 + 1], terminal[p0 + 1]) }

        for (p in 1..P) {
            val node = nodes[p - 1]
            if (parent[p] != 0)
                node.parent = nodes[parent[p] - 1]
            if (childLeft[p] != 0)
                node.childLeft = nodes[childLeft[p] - 1]
            if (childRight[p] != 0)
                node.childRight = nodes[childRight[p] - 1]
        }
    }

    inner class Node(
        val nodeType: NodeType,
        val terminalNumber: Int  // 1..X, or 0 if non-terminal
    ) {
        var parent: Node? = null
            internal set
        var childLeft: Node? = null
            internal set
        var childRight: Node? = null
            internal set

        val size: Int
            get() = 1 + (childLeft?.size ?: 0) + (childRight?.size ?: 0)
        //    when(nodeType) {
        //    NodeType.TERMINAL-> 1
        //    NodeType.AND ->1 + childLeft!!.size + childRight!!.size
        //}

        fun eval(inputValues: BooleanArray): Boolean {
            return when (nodeType) {
                NodeType.TERMINAL -> inputValues[terminalNumber - 1]
                NodeType.AND -> childLeft!!.eval(inputValues) && childRight!!.eval(inputValues)
                NodeType.OR -> childLeft!!.eval(inputValues) || childRight!!.eval(inputValues)
                NodeType.NOT -> !childLeft!!.eval(inputValues)
                NodeType.NONE -> error("Can't eval none-typed node")
            }
        }

        fun toSimpleString(): String {
            return when (nodeType) {
                NodeType.TERMINAL -> this@ParseTreeGuard.inputNames
                    ?.let { it[terminalNumber - 1] }
                    ?: "x$terminalNumber"
                NodeType.AND -> {
                    var left = childLeft!!.toSimpleString()
                    var right = childRight!!.toSimpleString()
                    if (childLeft!!.nodeType == NodeType.OR)
                        left = "($left)"
                    if (childRight!!.nodeType == NodeType.OR)
                        right = "($right)"
                    "$left & $right"
                }
                NodeType.OR -> {
                    var left = childLeft!!.toSimpleString()
                    var right = childRight!!.toSimpleString()
                    if (childLeft!!.nodeType == NodeType.AND)
                        left = "($left)"
                    if (childRight!!.nodeType == NodeType.AND)
                        right = "($right)"
                    "$left | $right"
                }
                NodeType.NOT -> {
                    var left = childLeft!!.toSimpleString()
                    if (childLeft!!.nodeType !in setOf(NodeType.TERMINAL, NodeType.NOT))
                        left = "($left)"
                    "~$left"
                }
                NodeType.NONE -> error("Why are you trying to display none-typed node?")
            }
        }

        fun toGraphvizString(): String {
            return toSimpleString()
        }

        fun toSmvString(): String {
            return when (nodeType) {
                NodeType.TERMINAL -> this.toSimpleString()
                NodeType.AND -> {
                    var left = childLeft!!.toSmvString()
                    var right = childRight!!.toSmvString()
                    if (childLeft!!.nodeType == NodeType.OR)
                        left = "($left)"
                    if (childRight!!.nodeType == NodeType.OR)
                        right = "($right)"
                    "$left & $right"
                }
                NodeType.OR -> {
                    var left = childLeft!!.toSmvString()
                    var right = childRight!!.toSmvString()
                    if (childLeft!!.nodeType == NodeType.AND)
                        left = "($left)"
                    if (childRight!!.nodeType == NodeType.AND)
                        right = "($right)"
                    "$left | $right"
                }
                NodeType.NOT -> {
                    var left = childLeft!!.toSmvString()
                    if (childLeft!!.nodeType !in setOf(NodeType.TERMINAL, NodeType.NOT))
                        left = "($left)"
                    "!$left"
                }
                NodeType.NONE -> error("Why are you trying to display none-typed node?")
            }
        }

        override fun toString(): String {
            return "Node(nodeType=$nodeType, terminalNumber=$terminalNumber, parent=$parent, childLeft=$childLeft, childRight=$childRight)"
        }
    }

    override fun eval(inputValues: BooleanArray): Boolean {
        return root.eval(inputValues)
    }

    override fun toSimpleString(): String {
        return root.toSimpleString()
    }

    override fun toGraphvizString(): String {
        return root.toGraphvizString()
    }

    override fun toSmvString(): String {
        return root.toSmvString()
    }

    override fun toString(): String {
        return "ParseTreeGuard(size=${nodes.size})"
    }

    companion object
}

enum class NodeType(val value: Int) {
    TERMINAL(1),
    AND(2),
    OR(3),
    NOT(4),
    NONE(5);

    companion object {
        private val lookup: Map<Int, NodeType> = NodeType.values().associateBy(NodeType::value)

        fun from(value: Int) = lookup[value]
    }
}
