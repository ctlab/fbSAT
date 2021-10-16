package ru.ifmo.fbsat.core.utils.serializers

import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import ru.ifmo.fbsat.core.automaton.guard.ParseTreeGuard

object ParseTreeGuardSerializer :
    KSerializer<ParseTreeGuard> by SerializerViaSurrogate(ParseTreeGuardSurrogate)

@Serializable
@SerialName("ParseTreeGuard")
private class ParseTreeGuardSurrogate private constructor(
    val expr: String,
) : Surrogate<ParseTreeGuard> {
    override fun toOriginal(): ParseTreeGuard = toParseTreeGuard()

    fun toParseTreeGuard(): ParseTreeGuard {
        TODO("Create ParseTreeGuard from string `expr`")
    }

    companion object Factory : Surrogate.Factory<ParseTreeGuard, ParseTreeGuardSurrogate> {
        override val surrogateSerializer: KSerializer<ParseTreeGuardSurrogate> = serializer()

        override fun from(original: ParseTreeGuard): ParseTreeGuardSurrogate {
            val expr = original.toSimpleString()
            return ParseTreeGuardSurrogate(
                expr = expr
            )
        }
    }
}
