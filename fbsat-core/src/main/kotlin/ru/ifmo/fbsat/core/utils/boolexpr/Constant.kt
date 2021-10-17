package ru.ifmo.fbsat.core.utils.boolexpr

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
sealed class Constant : BooleanExpression {
    @Serializable
    @SerialName("Constant.True")
    object True : Constant() {
        override fun eval(inputs: List<Boolean>): Boolean = true
        override fun stringify(): String = "⊤"
        override fun toString(): String = "Constant.True"
    }

    @Serializable
    @SerialName("Constant.False")
    object False : Constant() {
        override fun eval(inputs: List<Boolean>): Boolean = false
        override fun stringify(): String = "⊥"
        override fun toString(): String = "Constant.False"
    }
}
