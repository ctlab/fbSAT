package ru.ifmo.fbsat.core.utils.boolexpr

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

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

fun BooleanExpression.and(other: BooleanExpression): BinaryOperation {
    return BinaryOperation(BinaryOperation.Kind.And, this, other)
    // return BooleanExpression.and(this, other)
}

fun BooleanExpression.or(other: BooleanExpression): BinaryOperation {
    return BinaryOperation(BinaryOperation.Kind.Or, this, other)
    // return BooleanExpression.or(this, other)
}
