package ru.ifmo.fbsat.core.automaton.guard

import kotlinx.serialization.modules.SerializersModule
import kotlinx.serialization.modules.polymorphic
import kotlinx.serialization.modules.subclass
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.utils.pow
import ru.ifmo.fbsat.core.utils.toBinaryString

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

fun allInputValues(n: Int): Sequence<InputValues> =
    (0 until 2.pow(n)).asSequence().map { i ->
        InputValues(i.toString(2).padStart(n, '0'))
    }

fun Guard.truthTableString(inputNames: List<String>): String =
    allInputValues(inputNames.size).map { input ->
        if (this is TruthTableGuard) {
            this.truthTable[input]
        } else {
            eval(input)
        }
    }.asIterable().toBinaryString()
