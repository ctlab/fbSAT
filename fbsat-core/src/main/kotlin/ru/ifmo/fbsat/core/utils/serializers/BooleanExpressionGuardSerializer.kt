package ru.ifmo.fbsat.core.utils

import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import ru.ifmo.fbsat.core.automaton.BooleanExpressionGuard
import ru.ifmo.fbsat.core.utils.serializers.SerializerViaSurrogate
import ru.ifmo.fbsat.core.utils.serializers.Surrogate

object BooleanExpressionGuardSerializer :
    KSerializer<BooleanExpressionGuard> by SerializerViaSurrogate(BooleanExpressionGuardSurrogate)

@Serializable
@SerialName("BooleanExpressionGuard")
private class BooleanExpressionGuardSurrogate private constructor(
    val expr: String,
    val ast: BooleanExpression,
) : Surrogate<BooleanExpressionGuard> {
    override fun toOriginal(): BooleanExpressionGuard = toBooleanExpressionGuard()

    fun toBooleanExpressionGuard(): BooleanExpressionGuard = BooleanExpressionGuard(ast)

    companion object Factory : Surrogate.Factory<BooleanExpressionGuard, BooleanExpressionGuardSurrogate> {
        override val surrogateSerializer: KSerializer<BooleanExpressionGuardSurrogate> = serializer()

        override fun from(original: BooleanExpressionGuard): BooleanExpressionGuardSurrogate {
            val expr = original.toSimpleString()
            val ast = original.expr
            return BooleanExpressionGuardSurrogate(expr, ast)
        }
    }
}
