package ru.ifmo.fbsat.core.utils.boolexpr

import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json

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
