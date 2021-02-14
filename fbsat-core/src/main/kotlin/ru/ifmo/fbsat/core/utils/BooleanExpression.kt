package ru.ifmo.fbsat.core.utils

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import kotlin.random.Random
import kotlin.random.nextInt

@Serializable
sealed class BooleanExpression {
    abstract fun eval(inputs: List<Boolean>): Boolean
    abstract fun stringify(): String

    companion object
}

@Serializable
@SerialName("Variable")
data class Variable(
    val index: Int, // 0-based
    val name: String,
) : BooleanExpression() {
    override fun eval(inputs: List<Boolean>): Boolean = inputs[index]
    override fun stringify(): String = name
}

@Serializable
@SerialName("UnaryOperation")
data class UnaryOperation(
    val kind: Kind,
    val arg: BooleanExpression,
) : BooleanExpression() {
    override fun eval(inputs: List<Boolean>): Boolean = when (kind) {
        Kind.Not -> !arg.eval(inputs)
    }

    override fun stringify(): String {
        val s = arg.stringify()
        return when (kind) {
            Kind.Not -> "NOT $s"
        }
    }

    enum class Kind {
        Not;
    }
}

@Serializable
@SerialName("BinaryOperation")
data class BinaryOperation(
    val kind: Kind,
    val lhs: BooleanExpression,
    val rhs: BooleanExpression,
) : BooleanExpression() {
    override fun eval(inputs: List<Boolean>): Boolean = when (kind) {
        Kind.And -> lhs.eval(inputs) && rhs.eval(inputs)
        Kind.Or -> lhs.eval(inputs) || rhs.eval(inputs)
    }

    override fun stringify(): String {
        val left = lhs.stringify()
        val right = rhs.stringify()
        return "($left ${kind.name.toUpperCase()} $right)"
    }

    enum class Kind {
        And, Or;
    }
}

fun generateRandomBooleanExpression(
    size: Int,
    inputNames: List<String>,
    random: Random = Random,
): BooleanExpression {
    fun randomBooleanExpressionHelper(size: Int): BooleanExpression {
        require(size > 0) { "Size must be > 0." }
        return when (size) {
            1 -> {
                val i = random.nextInt(0, inputNames.size)
                Variable(i, inputNames[i])
            }
            2 -> {
                val child = randomBooleanExpressionHelper(1)
                UnaryOperation(UnaryOperation.Kind.Not, child)
            }
            else -> when (val t = listOf("Not", "And", "Or").random(random)) {
                "Not" -> {
                    val child = randomBooleanExpressionHelper(size - 1)
                    UnaryOperation(UnaryOperation.Kind.Not, child)
                }
                "And", "Or" -> {
                    // subtract 1 for the current (BinaryOperation) node
                    // subtract 1 more to ensure that rhs is at least of size 1
                    val leftSize = random.nextInt(1..(size - 2))
                    val lhs = randomBooleanExpressionHelper(leftSize)
                    val rhs = randomBooleanExpressionHelper(size - 1 - leftSize)
                    val kind = BinaryOperation.Kind.valueOf(t)
                    BinaryOperation(kind, lhs, rhs)
                }
                else -> error("Unexpected type $t")
            }
        }
    }
    return randomBooleanExpressionHelper(size)
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
    val s = Json { prettyPrint = true }.encodeToString(expr)
    println("serialized = $s")
    val expr2: BooleanExpression = Json.decodeFromString(s)
    println("expr2 = $expr2")
    println("expr2.stringify() = ${expr2.stringify()}")
}
