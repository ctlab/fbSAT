package ru.ifmo.fbsat.core.automaton.guard

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.Transient
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.utils.toBinaryString

@Serializable
@SerialName("TruthTableGuard")
class TruthTableGuard(
    val truthTable: Map<InputValues, Boolean?>,
) : Guard {
    override val size: Int = 0

    override fun eval(inputValues: InputValues): Boolean {
        return truthTable.getValue(inputValues)!!
    }

    override fun toSimpleString(): String {
        val s = truthTable.values.toBinaryString()
        return when {
            s.length <= 32 -> "[$s]"
            else -> "[${s.substring(0..15)}...]"
        }
    }

    override fun toGraphvizString(): String = toSimpleString()

    override fun toFbtString(): String = "TRUTH_TABLE"

    override fun toSmvString(): String = "TRUTH_TABLE"

    override fun toString(): String = "TruthTableGuard(truthTable = $truthTable)"
}
