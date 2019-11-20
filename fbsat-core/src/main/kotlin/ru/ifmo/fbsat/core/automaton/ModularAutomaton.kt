@file:Suppress("LocalVariableName")

package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import com.soywiz.klock.DateTime
import org.redundent.kotlin.xml.xml
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.Scenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.mutableListOfNulls
import ru.ifmo.fbsat.core.utils.random
import ru.ifmo.fbsat.core.utils.writeEventMerger
import java.io.File

@Suppress("PropertyName")
class ModularAutomaton(
    val modules: MultiArray<Automaton>, // [M] : Automaton
    val outputVariableModule: IntMultiArray // [Z] : 1..M
) {
    val M: Int = modules.shape[0]
    val Z: Int = outputVariableModule.shape[0]
    val moduleOutputVariables: MultiArray<List<Int>> = // [M] : {1..Z}
        MultiArray.create(M) { (m) ->
            (1..Z).filter { z -> outputVariableModule[z] == m }
        }

    val inputEvents: List<InputEvent> = modules.first().inputEvents
    val outputEvents: List<OutputEvent> = modules.first().outputEvents
    val inputNames: List<String> = modules.first().inputNames
    val outputNames: List<String> = (1..Z).map { z ->
        val m = outputVariableModule[z]
        modules[m].outputNames[moduleOutputVariables[m].indexOf(z)]
    }

    val numberOfTransitions: Int
        get() = modules.sumBy { it.numberOfTransitions }

    init {
        require(modules.all { it.inputEvents == inputEvents })
        require(modules.all { it.outputEvents == outputEvents })
        require(modules.all { it.inputNames == inputNames })
    }

    private fun gather(outputs: MultiArray<OutputValues>): OutputValues {
        return OutputValues(
            (1..Z).map { z ->
                val m = outputVariableModule[z]
                outputs[m][moduleOutputVariables[m].indexOf(z)]
            }
        )
    }

    private fun spread(outputValues: OutputValues): MultiArray<OutputValues> {
        return MultiArray.create(Z) { (m) ->
            OutputValues(moduleOutputVariables[m].map { outputValues[it - 1] })
        }
    }

    /**
     * Evaluate given [scenario].
     *
     * @return list of satisfying automaton states (i.e. states, by which each scenario element is satisfied).
     */
    private fun eval(scenario: Scenario): List<MultiArray<Automaton.EvalResult>?> {
        val results: MutableList<MultiArray<Automaton.EvalResult>?> = mutableListOfNulls(scenario.elements.size)

        val currentState: MultiArray<Automaton.State> = MultiArray.create(M) { (m) ->
            modules[m].initialState
        }
        val currentValues: MultiArray<OutputValues> = MultiArray.create(M) { (m) ->
            OutputValues.zeros(moduleOutputVariables[m].size)
        }

        for ((j, element) in scenario.elements.withIndex()) {
            val result = MultiArray.create(M) { (m) ->
                currentState[m].eval(
                    inputAction = element.inputAction,
                    currentValues = currentValues[m]
                )
            }
            val outputEvent = result.mapNotNull { it.outputAction.event }.firstOrNull()
            check(result.mapNotNull { it.outputAction.event }.all { it == outputEvent })
            val outputValues = gather(MultiArray.create(M) { (m) -> result[m].outputAction.values })

            if (OutputAction(outputEvent, outputValues) == element.outputAction) {
                results[j] = result
            } else {
                break
            }

            for (m in 1..M) {
                currentState[m] = result[m].destination
                currentValues[m] = result[m].outputAction.values
            }
        }

        return results
    }

    /**
     * Verify given [positiveScenario].
     *
     * @return `true` if [positiveScenario] is satisfied.
     */
    fun verify(positiveScenario: PositiveScenario): Boolean {
        val results: List<MultiArray<Automaton.EvalResult>?> = eval(positiveScenario)
        return results.last() != null
    }

    /**
     * Verify all positive scenarios in given [scenarioTree].
     *
     * @return `true` if **all** scenarios are satisfied.
     */
    fun verify(scenarioTree: ScenarioTree): Boolean =
        scenarioTree.scenarios.all(::verify)

    /**
     * Dump modular automaton in FBT format to [file].
     */
    fun dumpFbt(file: File, name: String? = null) {
        for (m in 1..M) {
            modules[m].dumpFbt(file.resolveSibling("module-$m.fbt"), "module-$m")
        }
        if (M != 2) writeEventMerger(file.resolveSibling("E_MERGE$M.fbt"), "E_MERGE$M")
        file.writeText(toFbtString(name))
    }

    /**
     * Stringify automaton to FBT format.
     */
    fun toFbtString(name: String? = null): String {
        fun r() = "%.3f".format((1.0..1000.0).random())

        return xml("FBType") {
            if (name != null) {
                attribute("Name", name)
            }
            "Identification"("Standard" to "61499-2")
            "VersionInfo"(
                "Organization" to "nxtControl GmbH",
                "Version" to "0.0",
                "Author" to "fbSAT",
                "Date" to DateTime.nowLocal().format("yyyy-MM-dd")
            )
            "InterfaceList" {
                "EventInputs" {
                    // Note: INIT input event has no associated variables
                    "Event"("Name" to "INIT")
                    for (inputEvent in inputEvents) {
                        "Event"("Name" to inputEvent.name) {
                            for (inputName in inputNames) {
                                "With"("Var" to inputName)
                            }
                        }
                    }
                }
                "EventOutputs" {
                    // Note: INITO output event has the same associated variables as all output events
                    for (outputEvent in outputEvents + OutputEvent("INITO")) {
                        "Event"("Name" to outputEvent.name) {
                            for (outputName in outputNames) {
                                "With"("Var" to outputName)
                            }
                        }
                    }
                }
                "InputVars" {
                    for (inputName in inputNames) {
                        "VarDeclaration"(
                            "Name" to inputName,
                            "Type" to "BOOL"
                        )
                    }
                }
                "OutputVars" {
                    for (outputName in outputNames) {
                        "VarDeclaration"(
                            "Name" to outputName,
                            "Type" to "BOOL"
                        )
                    }
                }
            }
            "FBNetwork" {
                for (m in 1..M) {
                    "FB"(
                        "Name" to "M$m",
                        "Type" to "module-$m",
                        "x" to r(), "y" to r()
                    )
                }
                for (outputEvent in outputEvents) {
                    for (m in 2..M) {
                        "FB"(
                            "Name" to "EM-${outputEvent.name}",
                            "Type" to if (M == 2) "E_MERGE" else "E_MERGE$M",
                            "x" to r(), "y" to r()
                        )
                    }
                }
                "EventConnections" {
                    for (inputEvent in inputEvents) {
                        for (m in 1..M) {
                            "Connection"(
                                "Source" to inputEvent.name,
                                "Destination" to "M$m.${inputEvent.name}"
                            )
                        }
                    }
                    for (outputEvent in outputEvents) {
                        for (m in 1..M) {
                            "Connection"(
                                "Source" to "M$m.${outputEvent.name}",
                                "Destination" to "EM-${outputEvent.name}.EI$m"
                            )
                            "Connection"(
                                "Source" to "EM-${outputEvent.name}.EO",
                                "Destination" to outputEvent.name
                            )
                        }
                    }
                }
                "DataConnections" {
                    for (inputName in inputNames) {
                        for (m in 1..M) {
                            "Connection"(
                                "Source" to inputName,
                                "Destination" to "M$m.$inputName"
                            )
                        }
                    }
                    for (m in 1..M) {
                        for (z in moduleOutputVariables[m]) {
                            val outputName = outputNames[z - 1]
                            "Connection"(
                                "Source" to "M$m.$outputName",
                                "Destination" to outputName
                            )
                        }
                    }
                }
            }
        }.toString(Globals.xmlPrintOptions)
    }
}
