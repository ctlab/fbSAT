package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.withIndex

@Suppress("PropertyName")
class ConsecutiveModularAutomaton(
    val modules: MultiArray<Automaton>,
    val inputEvents: List<InputEvent>,
    val outputEvents: List<OutputEvent>,
    val inputNames: List<String>,
    val outputNames: List<String>
) {
    val M: Int = modules.shape.single()
    val numberOfTransitions: Int = modules.values.sumBy { it.numberOfTransitions }
    val totalGuardsSize: Int = modules.values.sumBy { it.totalGuardsSize }

    constructor(modules: MultiArray<Automaton>, scenarioTree: OldPositiveScenarioTree) : this(
        modules,
        scenarioTree.inputEvents,
        scenarioTree.outputEvents,
        scenarioTree.inputNames,
        scenarioTree.outputNames
    )

    init {
        require(M >= 2)
        require(modules.values.all { it.inputEvents == inputEvents })
        require(modules.values.all { it.inputNames == inputNames })
        require(modules.values.all { it.outputEvents == outputEvents })
        require(modules.values.all { it.outputNames == outputNames })
    }

    fun verify(scenario: PositiveScenario): Boolean {
        val modularCurrentState = MultiArray.create(M) { (m) ->
            modules[m].initialState
        }
        var currentValues = OutputValues.zeros(outputNames.size)

        for ((i, element) in scenario.elements.withIndex(start = 1)) {
            // println("- - - - - - - -")
            // println("Current states: ${modularCurrentState.values.map { it.id }.joinToString(" ")}")
            // println("Current values: ${currentValues.values.toBinaryString()}")

            var result = modularCurrentState[1].eval(element.inputAction, currentValues)

            if (result.outputAction.event != null) {
                modularCurrentState[1] = result.destination
                currentValues = result.outputAction.values

                for (m in 2..M) {
                    if (element.outputEvent != null) {
                        if (result.outputAction.event != OutputEvent("CNF")) {
                            log.error("Module m = $m")
                            log.error("Scenario element #$i $element: FAILED")
                            log.error("Output event mismatch: ${result.outputAction.event} != CNF")
                            log.error("Result: $result")
                            log.error("Scenario: ${scenario.elements}")
                            return false
                        }
                    }
                    // else {
                    //     if (result.outputAction.event != null) {
                    //         log.error("Scenario element #$i: output event mismatch (${result.outputAction.event} != null)")
                    //         log.error("Scenario: ${scenario.elements}")
                    //         return false
                    //     }
                    // }
                    if (element.outputEvent == null) {
                        if (result.outputAction.values != currentValues) {
                            log.warn("Output values changed on epsilon event")
                        }
                    }

                    result = modularCurrentState[m].eval(
                        InputAction(
                            InputEvent(
                                "REQ"
                            ), element.inputValues
                        ),
                        currentValues
                    )
                    if (result.outputAction.event != null) {
                        modularCurrentState[m] = result.destination
                        currentValues = result.outputAction.values
                    } else {
                        break
                    }
                }
            }

            if (result.outputAction.event != element.outputEvent) {
                log.error("Scenario element #$i $element: FAILED")
                log.error("Output event mismatch: ${result.outputAction.event} != ${element.outputEvent}")
                log.error("Result: $result")
                log.error("Scenario: ${scenario.elements}")
                return false
            }
            if (result.outputAction.values != element.outputValues) {
                log.error("Scenario element #$i $element: FAILED")
                log.error("Output values mismatch: ${result.outputAction.values.values.toBinaryString()} != ${element.outputValues.values.toBinaryString()}")
                log.error("Result: $result")
                log.error("Scenario: ${scenario.elements}")
                return false
            }

            // log.success("Scenario element #$i ${element}: OK")
        }

        return true
    }

    fun verify(scenarioTree: OldPositiveScenarioTree): Boolean =
        scenarioTree.scenarios.all(::verify)
}
