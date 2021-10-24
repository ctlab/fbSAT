package ru.ifmo.fbsat.core.automaton.guard

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.utils.pow
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.toBooleanList
import kotlin.math.absoluteValue

@Serializable
@SerialName("ConjunctiveGuard")
class ConjunctiveGuard(
    val literals: List<Int>, // signed 1-based
    val inputNames: List<String>,
) : Guard {
    override val size: Int = literals.size + 1

    override fun eval(inputValues: InputValues): Boolean {
        return literals.all { lit ->
            inputValues[lit.absoluteValue - 1] xor (lit < 0)
        }
    }

    override fun toSimpleString(): String {
        return literals.joinToString(" & ") { lit ->
            (if (lit < 0) "~" else "") + inputNames[lit.absoluteValue - 1]
        }
    }

    override fun toGraphvizString(): String {
        return toSimpleString()
    }

    override fun toFbtString(): String {
        return toSimpleString().replace("~", "NOT ").replace(" & ", " AND ")
    }

    override fun toSmvString(): String {
        return toSimpleString().replace("~", "!")
    }
}
