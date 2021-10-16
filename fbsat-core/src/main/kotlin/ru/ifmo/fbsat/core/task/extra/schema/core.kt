package ru.ifmo.fbsat.core.task.extra.schema

import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.toBooleanArray
import ru.ifmo.fbsat.core.utils.toList_

data class Input(
    val values: List<Boolean>,
) {
    constructor(values: BooleanArray) :
        this(values.asList())

    constructor(values: Iterable<Boolean>) :
        this(values.toList_())

    constructor(values: String) :
        this(values.toBooleanArray())

    constructor(i: Int, numberOfVariables: Int) :
        this(i.toLong(), numberOfVariables)

    constructor(i: Long, numberOfVariables: Int) :
        this(List(numberOfVariables) { j -> 2L.pow(numberOfVariables - j - 1) and i != 0L })

    override fun toString(): String {
        return values.toBinaryString()
    }
}

data class Output(
    val values: List<Boolean>,
) {
    constructor(values: BooleanArray) :
        this(values.asList())

    constructor(values: Iterable<Boolean>) :
        this(values.toList_())

    constructor(values: String) :
        this(values.toBooleanArray())

    override fun toString(): String {
        return values.toBinaryString()
    }
}

private var iSchemaBlockType = 0

enum class SchemaBlockType(val value: Int) {
    AND(++iSchemaBlockType),
    OR(++iSchemaBlockType),
    NOT(++iSchemaBlockType),
    ;

    companion object {
        private val lookup: Map<Int, SchemaBlockType> = values().associateBy(SchemaBlockType::value)

        fun from(value: Int): SchemaBlockType = lookup.getValue(value)
    }
}

private var iSystemBlockType = 0

enum class SystemBlockType(
    val value: Int,
    val numberOfInputs: Int,
    val numberOfOutputs: Int,
    // TODO: numberOfStates: Int  (0 for stateless blocks)
    val hasState: Boolean,
) {
    AND(++iSystemBlockType, 2, 1, false),
    OR(++iSystemBlockType, 2, 1, false),
    NOT(++iSystemBlockType, 1, 1, false),
    HA(++iSystemBlockType, 2, 2, false),
    CNT(++iSystemBlockType, 1, 2, true),
    ;

    companion object {
        private val lookup: Map<Int, SystemBlockType> = values().associateBy(SystemBlockType::value)

        fun from(value: Int): SystemBlockType = lookup.getValue(value)
    }
}
