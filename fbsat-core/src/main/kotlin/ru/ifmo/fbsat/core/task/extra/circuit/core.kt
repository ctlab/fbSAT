package ru.ifmo.fbsat.core.task.extra.circuit

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

private var iBlockType = 0

enum class BlockType(val value: Int) {
    AND(++iBlockType),
    OR(++iBlockType),

    // XOR(++iBlockType),
    NOT(++iBlockType),
    // IMPLY(++iBlockType),
    ;

    companion object {
        private val lookup: Map<Int, BlockType> = values().associateBy(BlockType::value)

        fun from(value: Int): BlockType = lookup.getValue(value)
    }
}
