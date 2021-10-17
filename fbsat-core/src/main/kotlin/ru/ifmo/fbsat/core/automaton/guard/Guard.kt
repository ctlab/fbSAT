package ru.ifmo.fbsat.core.automaton.guard

import kotlinx.serialization.modules.SerializersModule
import kotlinx.serialization.modules.polymorphic
import kotlinx.serialization.modules.subclass
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.utils.pow
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.toBooleanList

@Suppress("PublicApiImplicitType")
val guardModule = SerializersModule {
    polymorphic(Guard::class) {
        subclass(UnconditionalGuard::class)
        subclass(TruthTableGuard::class)
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
}

fun Guard.truthTableString(inputNames: List<String>): String =
    (0 until 2.pow(inputNames.size)).map { i ->
        eval(InputValues(i.toString(2).padStart(inputNames.size, '0').toBooleanList()))
    }.toBinaryString()
