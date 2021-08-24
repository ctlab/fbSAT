package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.Transient
import kotlinx.serialization.modules.SerializersModule
import kotlinx.serialization.modules.polymorphic
import kotlinx.serialization.modules.subclass
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.utils.boolexpr.BinaryOperation
import ru.ifmo.fbsat.core.utils.boolexpr.BooleanExpression
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.boolexpr.Constant
import ru.ifmo.fbsat.core.utils.boolexpr.UnaryOperation
import ru.ifmo.fbsat.core.utils.boolexpr.Variable
import ru.ifmo.fbsat.core.utils.inputNamesPnP
import ru.ifmo.fbsat.core.utils.makeDnfString
import ru.ifmo.fbsat.core.utils.pow
import ru.ifmo.fbsat.core.utils.serializers.ParseTreeGuardSerializer
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.toBooleanList
import kotlin.math.absoluteValue

private val logger = MyLogger {}

@Suppress("PublicApiImplicitType")
val guardModule = SerializersModule {
    polymorphic(Guard::class) {
        subclass(UnconditionalGuard::class)
        subclass(TruthTableGuard::class)
        subclass(ParseTreeGuard::class)
        subclass(BooleanExpressionGuard::class)
    }
}

interface Guard {
    val size: Int
    fun eval(inputValues: InputValues): Boolean
    fun toSimpleString(): String
    fun toGraphvizString(): String
    fun toFbtString(): String
    fun toSmvString(): String

    // TODO: extract to extension method
    fun truthTableString(inputNames: List<String>): String
}

@Serializable
@SerialName("UnconditionalGuard")
class UnconditionalGuard : Guard {
    override val size: Int
        get() {
            // logger.warn("UnconditionalGuard has no meaningful size")
            // return 0
            return 1
        }

    override fun truthTableString(inputNames: List<String>): String {
        return "1"
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
}

@Serializable
@SerialName("TruthTableGuard")
class TruthTableGuard(
    val truthTable: Map<InputValues, Boolean?>,
) : Guard {
    override val size: Int = 0

    @Transient
    private val truthTableString = truthTable.values.filterNotNull().toBinaryString()

    override fun truthTableString(inputNames: List<String>): String = truthTableString

    override fun eval(inputValues: InputValues): Boolean {
        // return truthTable[uniqueInputs.indexOf(inputValues)] in "1x"
        // return truthTable.getValue(inputValues) ?: true
        return truthTable.getValue(inputValues)!!
    }

    override fun toSimpleString(): String {
        val s = truthTable.values.toBinaryString()
        return when {
            s.length <= 32 -> "[$s]"
            else -> "[${s.substring(0..15)}...]"
        }
    }

    override fun toGraphvizString(): String = toSimpleString()

    override fun toFbtString(): String = "TRUTH_TABLE"

    override fun toSmvString(): String = "TRUTH_TABLE"

    override fun toString(): String = "TruthTableGuard(truthTable = $truthTable)"
}

@Deprecated("Use BooleanExpressionGuard", ReplaceWith("BooleanExpressionGuard.from"))
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

fun BooleanExpression.size(): Int = when (this) {
    // is Constant -> 0
    is Constant -> 1
    // is Constant.False -> 0
    // is Constant.True -> 1
    // is Constant -> error("Are you sure?")
    is Variable -> 1
    is UnaryOperation -> 1 + arg.size()
    is BinaryOperation -> 1 + lhs.size() + rhs.size()
}

fun BooleanExpression.eval(inputValues: InputValues): Boolean = eval(inputValues.values)

fun BooleanExpression.toSimpleString(): String = when (this) {
    is Constant -> stringify()
    is Variable -> name
    is UnaryOperation -> {
        val s = arg.toSimpleString()
        when (kind) {
            UnaryOperation.Kind.Not -> "~$s"
        }
    }
    is BinaryOperation -> {
        val left = lhs.toSimpleString()
        val right = rhs.toSimpleString()
        when (kind) {
            BinaryOperation.Kind.And -> "($left & $right)"
            BinaryOperation.Kind.Or -> "($left | $right)"
        }
    }
}

fun BooleanExpression.toGraphvizString(): String = toSimpleString()

fun BooleanExpression.toFbtString(): String = when (this) {
    is Constant.True -> "TRUE"
    is Constant.False -> "FALSE"
    is Variable -> name
    is UnaryOperation -> {
        val s = arg.toFbtString()
        when (kind) {
            UnaryOperation.Kind.Not -> "NOT $s"
        }
    }
    is BinaryOperation -> {
        val left = lhs.toFbtString()
        val right = rhs.toFbtString()
        when (kind) {
            BinaryOperation.Kind.And -> "($left AND $right)"
            BinaryOperation.Kind.Or -> "($left OR $right)"
        }
    }
}

fun BooleanExpression.toSmvString(): String = when (this) {
    is Constant.True -> "True"
    is Constant.False -> "False"
    is Variable -> name
    is UnaryOperation -> {
        val s = arg.toSmvString()
        when (kind) {
            UnaryOperation.Kind.Not -> "!$s"
        }
    }
    is BinaryOperation -> {
        val left = lhs.toSmvString()
        val right = rhs.toSmvString()
        when (kind) {
            BinaryOperation.Kind.And -> "($left & $right)"
            BinaryOperation.Kind.Or -> "($left | $right)"
        }
    }
}

class BooleanExpressionGuard(
    val expr: BooleanExpression,
) : Guard {
    override val size: Int = expr.size()

    // FIXME: adhoc 10
    // FIXME: adhoc inputNamesPnP
    /*override*/ val truthTableString: String
        // get() = truthTableString((1..10).map { "x$it" })
        get() = truthTableString(inputNamesPnP)

    override fun truthTableString(inputNames: List<String>): String =
        (0 until 2.pow(inputNames.size)).map { i ->
            eval(InputValues(i.toString(2).padStart(inputNames.size, '0').toBooleanList()))
        }.toBinaryString()

    override fun eval(inputValues: InputValues): Boolean = expr.eval(inputValues)

    override fun toSimpleString(): String = expr.toSimpleString()
    override fun toGraphvizString(): String = expr.toGraphvizString()
    override fun toFbtString(): String = expr.toFbtString()
    override fun toSmvString(): String = expr.toSmvString()

    companion object {
        fun from(
            nodeType: MultiArray<NodeType>,
            terminal: IntMultiArray,
            parent: IntMultiArray,
            childLeft: IntMultiArray,
            childRight: IntMultiArray,
            inputNames: List<String>,
        ): BooleanExpressionGuard {
            val P = nodeType.shape.single()
            val nodes: MutableMap<Int, BooleanExpression> = mutableMapOf()
            for (p in P downTo 1) {
                val node = when (val t = nodeType[p]) {
                    NodeType.NONE -> {
                        // TODO: maybe check(nodes.size == 0),
                        //  because NONE-typed nodes are concentrated "in the end" of 1..P,
                        //  so when iterating from P to 1, we should not encounter any NONE-type node
                        //  after processing a not-NONE-typed one.
                        null
                    }
                    NodeType.TERMINAL -> {
                        val x = terminal[p]
                        check(x > 0) { "Terminal number of TERMINAL node must be a positive number" }
                        Variable(x - 1, inputNames[x - 1])
                    }
                    NodeType.AND, NodeType.OR -> {
                        val l = childLeft[p]
                        val r = childRight[p]
                        check(parent[l] == p) {
                            "Parent of left child mismatch (p=$p, parent[$l]=${parent[l]})"
                        }
                        check(parent[r] == p) {
                            "Parent of right child mismatch (p=$p, parent[$r]=${parent[r]})"
                        }
                        val left = nodes[l]
                            ?: error("Left child ($l) for p=$p is not created yet")
                        val right = nodes[r]
                            ?: error("Right child ($r) for p=$p is not created yet")
                        val kind = when (t) {
                            NodeType.AND -> BinaryOperation.Kind.And
                            NodeType.OR -> BinaryOperation.Kind.Or
                            else -> error("This should not happen")
                        }
                        BinaryOperation(kind, left, right)
                    }
                    NodeType.NOT -> {
                        val c = childLeft[p]
                        check(parent[c] == p) {
                            "Parent of child mismatch (p=$p, parent[$c]=${parent[c]})"
                        }
                        val arg = nodes[c]
                            ?: error("Child ($c) for p=$p is not created yet")
                        UnaryOperation(UnaryOperation.Kind.Not, arg)
                    }
                }
                if (node != null) nodes[p] = node
            }
            val expr = nodes[1] ?: error("Root (1) is not created yet")
            return BooleanExpressionGuard(expr)
        }
    }
}

class StringGuard(val expr: String, val inputNames: List<String>) : Guard {
    private val literals = expr.splitToSequence('&').map(String::trim).toList()

    override val size: Int =
        2 * literals.size - 1 + literals.count { it.startsWith("!") || it.startsWith("~") }

    override fun truthTableString(inputNames: List<String>): String {
        TODO()
    }

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

    override fun toString(): String {
        return "StringGuard(expr = $expr)"
    }
}

class DnfGuard(
    val dnf: List<List<String>>,
    val inputNames: List<String>,
) : Guard {
    // Note: `_dnf` is one-based and signed
    private val _dnf: List<List<Int>> =
        dnf.map { term ->
            term.map { s ->
                if (s.startsWith('!') || s.startsWith('~')) {
                    -(inputNames.indexOf(s.drop(1)) + 1)
                } else {
                    inputNames.indexOf(s) + 1
                }
            }
        }

    override val size: Int = dnf.sumOf { it.size }

    override fun truthTableString(inputNames: List<String>): String {
        TODO()
    }

    override fun eval(inputValues: InputValues): Boolean =
        if (_dnf.isEmpty()) {
            check(toSimpleString() == "0")
            false
        } else {
            _dnf.any { term ->
                term.all { Lit ->
                    (Lit < 0) xor inputValues.values[Lit.absoluteValue - 1]
                }
            }
        }

    override fun toSimpleString(): String {
        return makeDnfString(dnf, conjunction = "&", disjunction = " | ")
    }

    override fun toGraphvizString(): String {
        return toSimpleString()
    }

    override fun toFbtString(): String {
        return toSimpleString()
            .replace("|", " OR ")
            .replace("&", " AND ")
            .replace("~", "NOT ")
            .replace("!", "NOT ")
    }

    override fun toSmvString(): String {
        return toSimpleString()
    }

    override fun toString(): String {
        return "DnfGuard(dnf = $dnf)"
    }
}

enum class NodeType(val value: Int) {
    TERMINAL(1),
    AND(2),
    OR(3),
    NOT(4),
    NONE(5);

    companion object {
        private val lookup: Map<Int, NodeType> = values().associateBy(NodeType::value)

        fun from(value: Int): NodeType = lookup.getValue(value)
    }
}
