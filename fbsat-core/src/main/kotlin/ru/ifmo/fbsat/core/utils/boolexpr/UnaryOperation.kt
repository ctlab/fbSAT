package ru.ifmo.fbsat.core.utils.boolexpr

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

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

fun BooleanExpression.not() : UnaryOperation {
    // return UnaryOperation(UnaryOperation.Kind.Not, this)
    return BooleanExpression.not(this)
}
