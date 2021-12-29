package ru.ifmo.fbsat.core.task.extra.schema.stateful



private var iSystemBlockType = 0

enum class SystemBlockType(
    val value: Int,
    val numberOfInputs: Int,
    val numberOfOutputs: Int,
    val numberOfStates: Int, // (0 for stateless blocks)
    // val hasState: Boolean,
) {
    AND(++iSystemBlockType, 2, 1, 0),
    OR(++iSystemBlockType, 2, 1, 0),
    XOR(++iSystemBlockType, 2, 1, 0),
    NOT(++iSystemBlockType, 1, 1, 0),
    HA(++iSystemBlockType, 2, 2, 0),
    CNT(++iSystemBlockType, 1, 2, 3),
    ;

    val hasState: Boolean = numberOfStates > 0

    companion object {
        private val lookup: Map<Int, SystemBlockType> = values().associateBy(SystemBlockType::value)

        fun from(value: Int): SystemBlockType = lookup.getValue(value)
    }
}
