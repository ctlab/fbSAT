package ru.ifmo.fbsat.scenario

data class ScenarioElement(
    val inputEvent: String,
    val inputValues: String,
    val outputEvent: String?,
    val outputValues: String
) {
    var nodeId: Int? = null

    override fun toString(): String {
        return "Element($inputEvent[$inputValues] / $outputEvent[$outputValues])"
    }
}
