package ru.ifmo.fbsat.core.automaton.guard

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import kotlinx.serialization.Serializable
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.utils.boolexpr.BinaryOperation
import ru.ifmo.fbsat.core.utils.boolexpr.BooleanExpression
import ru.ifmo.fbsat.core.utils.boolexpr.Constant
import ru.ifmo.fbsat.core.utils.boolexpr.UnaryOperation
import ru.ifmo.fbsat.core.utils.boolexpr.Variable
import ru.ifmo.fbsat.core.utils.inputNamesPnP
import ru.ifmo.fbsat.core.utils.pow
import ru.ifmo.fbsat.core.utils.serializers.BooleanExpressionGuardSerializer
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.toBooleanList

@Serializable(with = BooleanExpressionGuardSerializer::class)
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
