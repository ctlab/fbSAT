package ru.ifmo.fbsat.core.utils.boolexpr

import kotlin.random.Random
import kotlin.random.nextInt

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
