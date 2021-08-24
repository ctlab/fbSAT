package ru.ifmo.fbsat.core.utils.boolexpr

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
@SerialName("Variable")
data class Variable(
    val index: Int, // 0-based
    val name: String,
) : BooleanExpression {
    override fun eval(inputs: List<Boolean>): Boolean = inputs[index]
    override fun stringify(): String = name
}
