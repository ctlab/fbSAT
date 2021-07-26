package ru.ifmo.fbsat.core.utils

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import kotlinx.serialization.modules.SerializersModule
import kotlinx.serialization.modules.polymorphic
import kotlinx.serialization.modules.subclass
import kotlin.random.Random
import kotlin.random.nextInt

@Suppress("PublicApiImplicitType")
val booleanExpressionModule = SerializersModule {
    polymorphic(BooleanExpression::class) {
        subclass(Variable::class)
        subclass(UnaryOperation::class)
        subclass(BinaryOperation::class)
    }
}

sealed interface BooleanExpression {
    fun eval(inputs: List<Boolean>): Boolean
    fun stringify(): String

    companion object {
        fun constant(value: Boolean): Constant {
            return when (value) {
                true -> Constant.True
                false -> Constant.False
            }
        }

        fun variable(index: Int, name: String): Variable {
            return Variable(index, name)
        }

        fun not(arg: BooleanExpression): UnaryOperation {
            return UnaryOperation(UnaryOperation.Kind.Not, arg)
        }

        fun and(lhs: BooleanExpression, rhs: BooleanExpression): BinaryOperation {
            return BinaryOperation(BinaryOperation.Kind.And, lhs, rhs)
        }

        fun or(lhs: BooleanExpression, rhs: BooleanExpression): BinaryOperation {
            return BinaryOperation(BinaryOperation.Kind.Or, lhs, rhs)
        }

        fun and(vararg args: BooleanExpression): BooleanExpression {
            return args.reduce { acc, expr -> and(acc, expr) }
        }

        fun or(vararg args: BooleanExpression): BooleanExpression {
            return args.reduce { acc, expr -> or(acc, expr) }
        }
    }
}

@Serializable
@SerialName("Constant")
sealed class Constant : BooleanExpression {
    object True : Constant() {
        override fun eval(inputs: List<Boolean>): Boolean = true
        override fun stringify(): String = "⊤"
    }

    object False : Constant() {
        override fun eval(inputs: List<Boolean>): Boolean = false
        override fun stringify(): String = "⊥"
    }
}

@Serializable
@SerialName("Variable")
data class Variable(
    val index: Int, // 0-based
    val name: String,
) : BooleanExpression {
    override fun eval(inputs: List<Boolean>): Boolean = inputs[index]
    override fun stringify(): String = name
}

@Serializable
@SerialName("UnaryOperation")
data class UnaryOperation(
    val kind: Kind,
    val arg: BooleanExpression,
) : BooleanExpression {
    override fun eval(inputs: List<Boolean>): Boolean {
        val value = arg.eval(inputs)
        return when (kind) {
            Kind.Not -> !value
        }
    }

    override fun stringify(): String {
        val s = arg.stringify()
        return when (kind) {
            Kind.Not -> "NOT $s"
        }
    }

    enum class Kind {
        Not
    }
}

@Serializable
@SerialName("BinaryOperation")
data class BinaryOperation(
    val kind: Kind,
    val lhs: BooleanExpression,
    val rhs: BooleanExpression,
) : BooleanExpression {
    override fun eval(inputs: List<Boolean>): Boolean {
        val left = lhs.eval(inputs)
        val right = rhs.eval(inputs)
        return when (kind) {
            Kind.And -> left && right
            Kind.Or -> left || right
        }
    }

    override fun stringify(): String {
        val left = lhs.stringify()
        val right = rhs.stringify()
        return "($left ${kind.name.uppercase()} $right)"
    }

    enum class Kind {
        And, Or
    }
}

fun randomBooleanExpression(
    size: Int,
    inputNames: List<String>,
    random: Random = Random,
): BooleanExpression {
    fun helper(size: Int): BooleanExpression {
        require(size > 0) { "Size must be > 0." }
        return when (size) {
            1 -> {
                val i = random.nextInt(0, inputNames.size)
                Variable(i, inputNames[i])
            }
            2 -> {
                val child = helper(1)
                UnaryOperation(UnaryOperation.Kind.Not, child)
            }
            else -> when (val t = listOf("Not", "And", "Or").random(random)) {
                "Not" -> {
                    val child = helper(size - 1)
                    UnaryOperation(UnaryOperation.Kind.Not, child)
                }
                "And", "Or" -> {
                    // why (size - 2) ?
                    //  subtract 1 for the current (BinaryOperation) node, and
                    //  subtract 1 more to ensure that rhs is at least of size 1
                    val leftSize = random.nextInt(1..(size - 2))
                    val lhs = helper(leftSize)
                    val rhs = helper(size - 1 - leftSize)
                    val kind = BinaryOperation.Kind.valueOf(t)
                    BinaryOperation(kind, lhs, rhs)
                }
                else -> error("Unexpected type $t")
            }
        }
    }
    return helper(size)
}

fun main() {
    val expr: BooleanExpression = BinaryOperation(
        BinaryOperation.Kind.And,
        Variable(0, "x1"),
        UnaryOperation(
            UnaryOperation.Kind.Not,
            BinaryOperation(
                BinaryOperation.Kind.Or,
                Variable(1, "x2"),
                Variable(2, "x3")
            )
        )
    )
    println("expr = $expr")
    println("expr.stringify() = ${expr.stringify()}")
    val json = Json {
        prettyPrint = true
        serializersModule = booleanExpressionModule
    }
    val s = json.encodeToString(expr)
    println("serialized = $s")
    val expr2: BooleanExpression = json.decodeFromString(s)
    println("expr2 = $expr2")
    println("expr2.stringify() = ${expr2.stringify()}")
    check(expr.stringify() == expr2.stringify())
}
