package ru.ifmo.fbsat.automaton

import ru.ifmo.fbsat.utils.toBinaryString
import ru.ifmo.fbsat.utils.toBooleanArray

interface Algorithm {
    fun eval(outputValues: BooleanArray): BooleanArray
    fun toSimpleString(): String
}

class BinaryAlgorithm(val algorithm0: BooleanArray, val algorithm1: BooleanArray) : Algorithm {

    constructor(algorithm0: String, algorithm1: String)
            : this(algorithm0.toBooleanArray(), algorithm1.toBooleanArray())

    init {
        require(algorithm0.size == algorithm1.size) { "Algorithm size mismatch (algo0 = $algorithm0, algo1 = $algorithm1)" }
    }

    override fun eval(outputValues: BooleanArray): BooleanArray {
        require(outputValues.size == algorithm0.size) { "Number (${outputValues.size}) of output values (${outputValues.toList()}) and algorithm size (${algorithm0.size}) mismatch" }
        return outputValues
            .mapIndexed { i, b -> if (b) algorithm1[i] else algorithm0[i] }
            .toBooleanArray()
    }

    override fun toSimpleString(): String {
        return "0:${algorithm0.toBinaryString()}, 1:${algorithm1.toBinaryString()}"
    }

    override fun toString(): String {
        return "BinaryAlgorithm(${this.toSimpleString()})"
    }
}
