package ru.ifmo.fbsat.core.automaton.guard

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import ru.ifmo.fbsat.core.scenario.InputValues

@Serializable
@SerialName("UnconditionalGuard")
class UnconditionalGuard : Guard {
    override val size: Int
        get() {
            // logger.warn("UnconditionalGuard has no meaningful size")
            // return 0
            return 1
        }

    override fun eval(inputValues: InputValues): Boolean {
        return true
    }

    override fun toSimpleString(): String {
        return "1"
    }

    override fun toGraphvizString(): String {
        return "1"
    }

    override fun toFbtString(): String {
        return "1"
    }

    override fun toSmvString(): String {
        return "1"
    }

    override fun toString(): String {
        return "UnconditionalGuard()"
    }
}
