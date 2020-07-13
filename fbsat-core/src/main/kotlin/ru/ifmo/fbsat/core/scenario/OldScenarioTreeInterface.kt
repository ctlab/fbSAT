package ru.ifmo.fbsat.core.scenario

@Deprecated("this is old")
interface OldScenarioTreeInterface {
    val size: Int
    val inputEvents: List<InputEvent>
    val outputEvents: List<OutputEvent>
    val inputNames: List<String>
    val outputNames: List<String>
    val uniqueInputs: List<InputValues>
    val uniqueOutputs: List<OutputValues>

    fun parent(v: Int): Int
    fun previousActive(v: Int): Int
    fun inputEvent(v: Int): Int
    fun outputEvent(v: Int): Int
    fun inputNumber(v: Int): Int
    fun outputNumber(v: Int): Int
    fun inputValue(v: Int, x: Int): Boolean
    fun outputValue(v: Int, z: Int): Boolean
}
