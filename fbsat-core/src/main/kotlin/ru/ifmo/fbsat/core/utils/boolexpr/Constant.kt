package ru.ifmo.fbsat.core.utils.boolexpr

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
@SerialName("Constant")
sealed class Constant : BooleanExpression {
    object True : Constant() {
        override fun eval(inputs: List<Boolean>): Boolean = true
        override fun stringify(): String = "⊤"
    }

    object False : Constant() {
        override fun eval(inputs: List<Boolean>): Boolean = false
        override fun stringify(): String = "⊥"
    }
}
