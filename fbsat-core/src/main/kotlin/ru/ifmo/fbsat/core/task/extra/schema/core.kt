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
    val isUnary: Boolean,
    val isBinary: Boolean,
    val hasState: Boolean,
) {
    AND(++iSystemBlockType, false, true, false),
    OR(++iSystemBlockType, false, true, false),
    NOT(++iSystemBlockType, true, false, false),
    FF(++iSystemBlockType, false, false, true),
    ;

    companion object {
        private val lookup: Map<Int, SystemBlockType> = values().associateBy(SystemBlockType::value)

        fun from(value: Int): SystemBlockType = lookup.getValue(value)
    }
}
