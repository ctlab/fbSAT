package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.MutableMultiArray
import com.github.lipen.multiarray.map
import com.github.lipen.satlib.core.Context
import com.github.lipen.satlib.core.Model
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.utils.ModularContext
import ru.ifmo.fbsat.core.utils.mylog
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.withIndex

@Suppress("PropertyName")
class ConsecutiveModularAutomaton(
    val modules: MultiArray<Automaton>,
    val inputEvents: List<InputEvent>,
    val outputEvents: List<OutputEvent>,
    val inputNames: List<String>,
    val outputNames: List<String>,
) {
    val M: Int = modules.shape.single()
    val numberOfModules: Int = M
    val numberOfStates: Int = modules.values.sumBy { it.numberOfStates }
    val numberOfTransitions: Int = modules.values.sumBy { it.numberOfTransitions }
    val totalGuardsSize: Int = modules.values.sumBy { it.totalGuardsSize }

    constructor(modules: MultiArray<Automaton>, scenarioTree: PositiveScenarioTree) : this(
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
        val modularCurrentState = MutableMultiArray.new(M) { (m) ->
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
                            mylog.error("Module m = $m")
                            mylog.error("Scenario element #$i $element: FAILED")
                            mylog.error("Output event mismatch: ${result.outputAction.event} != CNF")
                            mylog.error("Result: $result")
                            mylog.error("Scenario: ${scenario.elements}")
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
                            mylog.warn("Output values changed on epsilon event")
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
                mylog.error("Scenario element #$i $element: FAILED")
                mylog.error("Output event mismatch: ${result.outputAction.event} != ${element.outputEvent}")
                mylog.error("Result: $result")
                mylog.error("Scenario: ${scenario.elements}")
                return false
            }
            if (result.outputAction.values != element.outputValues) {
                mylog.error("Scenario element #$i $element: FAILED")
                mylog.error("Output values mismatch: ${result.outputAction.values.values.toBinaryString()} != ${element.outputValues.values.toBinaryString()}")
                mylog.error("Result: $result")
                mylog.error("Scenario: ${scenario.elements}")
                return false
            }

            // log.success("Scenario element #$i ${element}: OK")
        }

        return true
    }

    fun verify(scenarioTree: OldPositiveScenarioTree): Boolean =
        scenarioTree.scenarios.all(::verify)

    fun getStats(): String {
        return "M = $numberOfModules, " +
            "C = ${modules.values.joinToString("+") { it.numberOfStates.toString() }} = $numberOfStates, " +
            "K = ${modules.values.map { it.maxOutgoingTransitions }}, " +
            "P = ${modules.values.map { it.maxGuardSize }}, " +
            "T = ${modules.values.joinToString("+") { it.numberOfTransitions.toString() }} = $numberOfTransitions, " +
            "N = ${modules.values.joinToString("+") { it.totalGuardsSize.toString() }} = $totalGuardsSize"
    }

    fun printStats() {
        mylog.just("    " + getStats())
    }
}

fun buildBasicConsecutiveModularAutomaton(
    context: Context,
    model: Model,
): ConsecutiveModularAutomaton {
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val modularContext: ModularContext = context["modularContext"]
    val modules = modularContext.map { buildBasicAutomaton(it, model) }

    return ConsecutiveModularAutomaton(modules, scenarioTree)
}

fun buildExtendedConsecutiveModularAutomaton(
    context: Context,
    model: Model,
): ConsecutiveModularAutomaton {
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val modularContext: ModularContext = context["modularContext"]
    val modules = modularContext.map { buildExtendedAutomaton(it, model) }

    return ConsecutiveModularAutomaton(modules, scenarioTree)
}
