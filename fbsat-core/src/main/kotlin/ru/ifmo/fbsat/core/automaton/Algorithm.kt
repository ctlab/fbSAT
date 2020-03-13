package ru.ifmo.fbsat.core.automaton

import ru.ifmo.fbsat.core.utils.toBinaryString

interface Algorithm {
    fun eval(outputValues: OutputValues): OutputValues
    fun toSimpleString(): String
}

class BinaryAlgorithm(val algorithm0: BooleanArray, val algorithm1: BooleanArray) : Algorithm {
    constructor(algorithm0: Collection<Boolean>, algorithm1: Collection<Boolean>) :
        this(algorithm0.toBooleanArray(), algorithm1.toBooleanArray())

    init {
        require(algorithm0.size == algorithm1.size) {
            "Algorithm size mismatch (algo0 = $algorithm0, algo1 = $algorithm1)"
        }
    }

    override fun eval(outputValues: OutputValues): OutputValues {
        require(outputValues.values.size == algorithm0.size) { "Number (${outputValues.values.size}) of output values (${outputValues.values.toList()}) and algorithm size (${algorithm0.size}) mismatch" }
        return OutputValues(outputValues.values.mapIndexed { i, b -> if (b) algorithm1[i] else algorithm0[i] })
    }

    override fun toSimpleString(): String {
        return "0:${algorithm0.toBinaryString()}, 1:${algorithm1.toBinaryString()}"
    }

    fun toFbtString(): String {
        return "${algorithm0.toBinaryString()}_${algorithm1.toBinaryString()}"
    }

    fun toST(outputNames: List<String>): String {
        require(outputNames.size == algorithm0.size) { "Wrong number of output names" }

        return outputNames.indices.joinToString("") { i ->
            val name = outputNames[i]
            val a0 = algorithm0[i]
            val a1 = algorithm1[i]
            if (a0 == a1)
                "$name:=${a0.toString().toUpperCase()};"
            else
                "$name:=NOT $name;"
        }
    }

    override fun toString(): String {
        return "BinaryAlgorithm(${this.toSimpleString()})"
    }
}
