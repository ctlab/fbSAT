package ru.ifmo.fbsat.core.utils

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.Scenario

fun Automaton.checkMapping(
    scenarios: List<Scenario>,
    mapping: IntMultiArray
): Boolean {
    var isOk = true
    for ((i, scenario) in scenarios.withIndex(start = 1)) {
        val automatonMapping = getMapping(scenario).map { it?.id ?: 0 }
        val assignmentMapping = scenario.elements.map { mapping[it.nodeId!!] }
        if (automatonMapping != assignmentMapping) {
            log.error("Scenario $i/${scenarios.size} mapping mismatch:")
            log.error("Automaton mapping:  ${automatonMapping.joinToString(" ")}")
            log.error("Assignment mapping: ${assignmentMapping.joinToString(" ")}")
            isOk = false
        }
    }
    return isOk
}
