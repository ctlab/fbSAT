package ru.ifmo.fbsat.core.automaton.guard

import kotlinx.serialization.modules.SerializersModule
import kotlinx.serialization.modules.polymorphic
import kotlinx.serialization.modules.subclass
import ru.ifmo.fbsat.core.scenario.InputValues

@Suppress("PublicApiImplicitType")
val guardModule = SerializersModule {
    polymorphic(Guard::class) {
        subclass(UnconditionalGuard::class)
        subclass(TruthTableGuard::class)
        subclass(ParseTreeGuard::class)
        subclass(BooleanExpressionGuard::class)
    }
}

interface Guard {
    val size: Int
    fun eval(inputValues: InputValues): Boolean
    fun toSimpleString(): String
    fun toGraphvizString(): String
    fun toFbtString(): String
    fun toSmvString(): String

    // TODO: extract to extension method
    fun truthTableString(inputNames: List<String>): String
}
