package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.toBinaryString

interface Guard {
    val size: Int
    fun eval(inputValues: InputValues): Boolean
    fun toSimpleString(): String
    fun toGraphvizString(): String
    fun toFbtString(): String
    fun toSmvString(): String
}

class UnconditionalGuard : Guard {
    override val size: Int
        get() {
            log.warn("UnconditionalGuard has no meaningful size")
            return 0
        }

    override fun eval(inputValues: InputValues): Boolean {
        return true
    }

    override fun toSimpleString(): String {
        return "1"
    }

    override fun toGraphvizString(): String {
        return "1"
    }

    override fun toFbtString(): String {
        return "1"
    }

    override fun toSmvString(): String {
        return "1"
    }

    override fun toString(): String {
        return "UnconditionalGuard()"
    }

    // Allow companion object extensions
    companion object
}

class TruthTableGuard(
    val truthTable: Map<InputValues, Boolean?>,
    private val inputNames: List<String>,
    private val uniqueInputs: List<InputValues>
) : Guard {
    override val size: Int
        get() {
            log.warn("TruthTableGuard has no meaningful size")
            return 0
        }

    init {
        // require(truthTable.size == uniqueInputs.size) {
        //     "Truth table size and number of unique inputs mismatch"
        // }
    }

    override fun eval(inputValues: InputValues): Boolean {
        // return truthTable[uniqueInputs.indexOf(inputValues)] in "1x"
        return truthTable.getValue(inputValues) ?: true
    }

    override fun toSimpleString(): String {
        return "[${truthTable.values.toList().toBinaryString()}]"
    }

    override fun toGraphvizString(): String =
        if (truthTable.size <= 8)
            "[${truthTable.values.toList().toBinaryString()}]"
        else
            "[${truthTable.values.take(3).toBinaryString()}${Typography.ellipsis}]"

    override fun toFbtString(): String {
        return truthTable.entries
            .filter { (_, value) -> value == true }
            .joinToString(" OR ") { (input, _) ->
                "(" + inputNames.zip(input.values).joinToString(" AND ") { (name, value) ->
                    if (value) name
                    else "NOT $name"
                } + ")"
            }
    }

    override fun toSmvString(): String {
        return "TRUTH_TABLE"
        // return truthTable
        //     .asIterable()
        //     .zip(uniqueInputs)
        //     // .filter { it.first == '1' }
        //     .filter { it.first in "1x" }
        //     .joinToString(" | ", prefix = "(", postfix = ")") { (_, input) ->
        //         input
        //             .asIterable()
        //             .zip(inputNames)
        //             .joinToString("&") { (value, name) ->
        //                 when (value) {
        //                     '1' -> name
        //                     '0' -> "!$name"
        //                     else -> throw Exception("...")
        //                 }
        //             }
        //     }
    }

    override fun toString(): String {
        return "TruthTableGuard(tt = $truthTable)"
    }

    // Allow companion object extensions
    companion object
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
        val terminalNumber: Int // 1..X, or 0 if non-terminal
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

    // Allow companion object extensions
    companion object
}

class StringGuard(val expr: String, val inputNames: List<String>) : Guard {
    private val literals = expr.splitToSequence('&').map(String::trim).toList()

    override val size: Int =
        2 * literals.size - 1 + literals.count { it.startsWith("!") || it.startsWith("~") }

    override fun eval(inputValues: InputValues): Boolean {
        return literals.all {
            if (it.startsWith("!") || it.startsWith("~"))
                !inputValues[inputNames.indexOf(it.substring(1))]
            else
                inputValues[inputNames.indexOf(it)]
        }
    }

    override fun toSimpleString(): String {
        return expr
    }

    override fun toGraphvizString(): String {
        return expr
    }

    override fun toFbtString(): String {
        return expr
            .replace("&", " AND ")
            .replace("~", "NOT ")
            .replace("!", "NOT ")
    }

    override fun toSmvString(): String {
        return expr
    }

    // Allow companion object extensions
    companion object
}

enum class NodeType(val value: Int) {
    TERMINAL(1),
    AND(2),
    OR(3),
    NOT(4),
    NONE(5);

    companion object {
        private val lookup: Map<Int, NodeType> = values().associateBy(NodeType::value)

        fun from(value: Int) = lookup[value]
    }
}
