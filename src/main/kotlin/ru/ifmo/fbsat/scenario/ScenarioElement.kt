package ru.ifmo.fbsat.scenario

class ScenarioElement(
    val inputEvent: String,
    val inputValues: String,
    val outputEvent: String?,
    val outputValues: String,
    var ceState: String? = null // FIXME: remove
) {
    var nodeId: Int? = null

    override fun toString(): String {
        return "Element($inputEvent[$inputValues] / $outputEvent[$outputValues])"
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as ScenarioElement

        if (inputEvent != other.inputEvent) return false
        if (inputValues != other.inputValues) return false
        if (outputEvent != other.outputEvent) return false
        if (outputValues != other.outputValues) return false

        return true
    }

    override fun hashCode(): Int {
        var result = inputEvent.hashCode()
        result = 31 * result + inputValues.hashCode()
        result = 31 * result + (outputEvent?.hashCode() ?: 0)
        result = 31 * result + outputValues.hashCode()
        return result
    }
}
