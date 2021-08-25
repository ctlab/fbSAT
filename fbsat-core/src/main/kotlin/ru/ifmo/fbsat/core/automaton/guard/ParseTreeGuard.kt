package ru.ifmo.fbsat.core.automaton.guard

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import kotlinx.serialization.Serializable
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.utils.pow
import ru.ifmo.fbsat.core.utils.serializers.ParseTreeGuardSerializer
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.toBooleanList

@Deprecated("Use BooleanExpressionGuard")
@Serializable(with = ParseTreeGuardSerializer::class)
class ParseTreeGuard(
    nodeType: MultiArray<NodeType>,
    terminal: IntMultiArray,
    parent: IntMultiArray,
    childLeft: IntMultiArray,
    childRight: IntMultiArray,
    private val inputNames: List<String>? = null,
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

    override fun truthTableString(inputNames: List<String>): String =
        (0 until 2.pow(inputNames.size)).map { i ->
            eval(InputValues(i.toString(2).padStart(inputNames.size, '0').toBooleanList()))
        }.toBinaryString()

    inner class Node(
        val nodeType: NodeType,
        val terminalNumber: Int, // 1..X, or 0 if non-terminal
    ) {
        var parent: Node? = null
            internal set
        var childLeft: Node? = null
            internal set
        var childRight: Node? = null
            internal set

        val size: Int
            get() = 1 + (childLeft?.size ?: 0) + (childRight?.size ?: 0)

        init {
            if (nodeType == NodeType.TERMINAL)
                require(terminalNumber > 0) { "Terminal number of terminal node must be > 0" }
            else
                require(terminalNumber == 0) { "Terminal number of non-terminal node must be = 0" }
        }

        fun eval(inputValues: InputValues): Boolean {
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
                    if (childLeft!!.nodeType !in setOf(
                            NodeType.TERMINAL,
                            NodeType.NOT
                        )
                    )
                        left = "($left)"
                    "~$left"
                }
                NodeType.NONE -> error("Why are you trying to display none-typed node?")
            }
        }

        fun toGraphvizString(): String {
            return toSimpleString()
        }

        fun toFbtString(): String {
            return when (nodeType) {
                NodeType.TERMINAL -> this.toSimpleString()
                NodeType.AND -> {
                    var left = childLeft!!.toFbtString()
                    var right = childRight!!.toFbtString()
                    if (childLeft!!.nodeType == NodeType.OR)
                        left = "($left)"
                    if (childRight!!.nodeType == NodeType.OR)
                        right = "($right)"
                    "$left AND $right"
                }
                NodeType.OR -> {
                    var left = childLeft!!.toFbtString()
                    var right = childRight!!.toFbtString()
                    // if (childLeft!!.nodeType == NodeType.AND)
                    //     left = "($left)"
                    // if (childRight!!.nodeType == NodeType.AND)
                    //     right = "($right)"
                    "$left | $right"
                }
                NodeType.NOT -> {
                    val left = childLeft!!.toFbtString()
                    if (childLeft!!.nodeType in setOf(
                            NodeType.TERMINAL,
                            NodeType.NOT
                        )
                    )
                        "NOT $left"
                    else
                        "NOT($left)"
                }
                NodeType.NONE -> error("Why are you trying to display none-typed node?")
            }
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
                    if (childLeft!!.nodeType !in setOf(
                            NodeType.TERMINAL,
                            NodeType.NOT
                        )
                    )
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

    override fun eval(inputValues: InputValues): Boolean {
        return root.eval(inputValues)
    }

    override fun toSimpleString(): String {
        return root.toSimpleString()
    }

    override fun toGraphvizString(): String {
        return root.toGraphvizString()
    }

    override fun toFbtString(): String {
        return root.toFbtString()
    }

    override fun toSmvString(): String {
        return root.toSmvString()
    }

    override fun toString(): String {
        return "ParseTreeGuard(size=${nodes.size})"
    }
}
