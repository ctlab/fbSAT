@file:Suppress("LocalVariableName")

package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.MutableMultiArray
import com.github.lipen.multiarray.mapIndexed
import com.github.lipen.satlib.core.Context
import com.github.lipen.satlib.core.Model
import com.soywiz.klock.DateTime
import org.redundent.kotlin.xml.xml
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.Scenario
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.convertBoolVarArray
import ru.ifmo.fbsat.core.solver.convertDomainVarArray
import ru.ifmo.fbsat.core.solver.convertIntVarArray
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.ModularContext
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.mutableListOfNulls
import ru.ifmo.fbsat.core.utils.random
import ru.ifmo.fbsat.core.utils.withIndex
import ru.ifmo.fbsat.core.utils.writeEventMerger
import java.io.File

private val logger = MyLogger {}

@Suppress("MemberVisibilityCanBePrivate", "PropertyName")
class ParallelModularAutomaton(
    val modules: MultiArray<Automaton>, // [M] : Automaton
    val outputVariableModule: IntMultiArray, // [Z] : 1..M
) {
    val M: Int = modules.shape.single()
    val Z: Int = outputVariableModule.shape.single()
    val moduleOutputVariables: MultiArray<List<Int>> = // [M] : {1..Z}
        MultiArray.new(M) { (m) ->
            (1..Z).filter { z -> outputVariableModule[z] == m }
        }

    val inputEvents: List<InputEvent> = modules.values.first().inputEvents
    val outputEvents: List<OutputEvent> = modules.values.first().outputEvents
    val inputNames: List<String> = modules.values.first().inputNames
    val outputNames: List<String> = (1..Z).map { z ->
        val m = outputVariableModule[z]
        modules[m].outputNames[moduleOutputVariables[m].indexOf(z)]
    }

    val numberOfModules: Int = M
    val numberOfStates: Int = modules.values.sumBy { it.numberOfStates }
    val numberOfTransitions: Int = modules.values.sumBy { it.numberOfTransitions }
    val totalGuardsSize: Int = modules.values.sumBy { it.totalGuardsSize }

    init {
        require(modules.values.all { it.inputEvents == inputEvents })
        require(modules.values.all { it.outputEvents == outputEvents })
        require(modules.values.all { it.inputNames == inputNames })
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
        return MultiArray.new(Z) { (m) ->
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

        val currentState: MutableMultiArray<Automaton.State> = MutableMultiArray.new(M) { (m) ->
            modules[m].initialState
        }
        val currentValues: MutableMultiArray<OutputValues> = MutableMultiArray.new(M) { (m) ->
            OutputValues.zeros(moduleOutputVariables[m].size)
        }

        for ((j, element) in scenario.elements.withIndex()) {
            val result = MultiArray.new(M) { (m) ->
                currentState[m].eval(
                    inputAction = element.inputAction,
                    currentValues = currentValues[m]
                )
            }
            val outputEvent = result.values.mapNotNull { it.outputAction.event }.firstOrNull()
            check(result.values.mapNotNull { it.outputAction.event }.all { it == outputEvent })
            val outputValues = gather(MultiArray.new(M) { (m) -> result[m].outputAction.values })

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
    fun verify(scenarioTree: OldPositiveScenarioTree): Boolean =
        scenarioTree.scenarios.all(::verify)

    /**
     * Dump modular automaton in FBT format to [file].
     */
    fun dumpFbt(file: File, name: String? = null) {
        for (m in 1..M) {
            modules[m].dumpFbt(file.resolveSibling("module_$m.fbt"), "module_$m")
        }
        if (M != 2) writeEventMerger(M, file.resolveSibling("E_MERGE$M.fbt"), "E_MERGE$M")
        file.writeText(toFbtString(name))
    }

    fun getStats(): String {
        return "M = $numberOfModules, " +
            "C = ${modules.values.joinToString("+") { it.numberOfStates.toString() }} = $numberOfStates, " +
            "K = ${modules.values.map { it.maxOutgoingTransitions }}, " +
            "P = ${modules.values.map { it.maxGuardSize }}, " +
            "T = ${modules.values.joinToString("+") { it.numberOfTransitions.toString() }} = $numberOfTransitions, " +
            "N = ${modules.values.joinToString("+") { it.totalGuardsSize.toString() }} = $totalGuardsSize"
    }

    fun printStats() {
        logger.info("    " + getStats())
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
            attribute("Namespace", "Main")
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
                        "Type" to "module_$m",
                        "Namespace" to "Main",
                        "x" to r(), "y" to r()
                    )
                }
                for (outputEvent in outputEvents) {
                    for (m in 2..M) {
                        "FB"(
                            "Name" to "EM_${outputEvent.name}",
                            "Type" to if (M == 2) "E_MERGE" else "E_MERGE$M",
                            "Namespace" to if (M == 2) "IEC61499.Standard" else "Main",
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
                                "Destination" to "EM_${outputEvent.name}.EI$m"
                            )
                        }
                        "Connection"(
                            "Source" to "EM_${outputEvent.name}.EO",
                            "Destination" to outputEvent.name
                        )
                    }
                    // Handle initialization
                    "Connection"(
                        "Source" to "INIT",
                        "Destination" to "M1.INIT"
                    )
                    for (m in 1 until M) {
                        "Connection"(
                            "Source" to "M$m.INITO",
                            "Destination" to "M${m + 1}.INIT"
                        )
                    }
                    "Connection"(
                        "Source" to "M$M.INITO",
                        "Destination" to "INITO"
                    )
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

fun buildBasicParallelModularAutomaton(
    context: Context,
    model: Model,
): ParallelModularAutomaton {
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val modularContext: ModularContext = context["modularContext"]
    val moduleControllingOutputVariable = context.convertIntVarArray("moduleControllingOutputVariable", model)

    val modules = modularContext.mapIndexed { (m), ctx ->
        val C: Int = ctx["C"]
        val K: Int = ctx["K"]
        val Z: Int = ctx["Z"]
        val transitionDestination = ctx.convertIntVarArray("transitionDestination", model)
        val transitionInputEvent = ctx.convertIntVarArray("transitionInputEvent", model)
        val transitionTruthTable = ctx.convertBoolVarArray("transitionTruthTable", model)
        val stateOutputEvent = ctx.convertIntVarArray("stateOutputEvent", model)
        val stateAlgorithmBot = ctx.convertBoolVarArray("stateAlgorithmBot", model)
        val stateAlgorithmTop = ctx.convertBoolVarArray("stateAlgorithmTop", model)
        val moduleOutputVariables = (1..Z).filter { z -> moduleControllingOutputVariable[z] == m }

        Automaton(
            inputEvents = scenarioTree.inputEvents,
            outputEvents = scenarioTree.outputEvents,
            inputNames = scenarioTree.inputNames,
            outputNames = moduleOutputVariables.map { z -> scenarioTree.outputNames[z - 1] }
        ).endow(
            C = C, K = K,
            stateOutputEvent = { c ->
                stateOutputEvent[c].let { o ->
                    if (o == 0) null
                    else scenarioTree.outputEvents[o - 1]
                }
            },
            stateAlgorithm = { c ->
                BinaryAlgorithm(
                    algorithm0 = moduleOutputVariables.map { z -> stateAlgorithmBot[c, z] },
                    algorithm1 = moduleOutputVariables.map { z -> stateAlgorithmTop[c, z] }
                )
            },
            transitionDestination = { c, k ->
                transitionDestination[c, k]
            },
            transitionInputEvent = { c, k ->
                scenarioTree.inputEvents[transitionInputEvent[c, k] - 1]
            },
            transitionGuard = { c, k ->
                TruthTableGuard(
                    truthTable = scenarioTree.uniqueInputs
                        .withIndex(start = 1)
                        .associate { (u, input) ->
                            input to transitionTruthTable[c, k, u]
                        }
                    // inputNames = scenarioTree.inputNames,
                    // uniqueInputs = scenarioTree.uniqueInputs
                )
            }
        )
    }

    return ParallelModularAutomaton(modules, moduleControllingOutputVariable)
}

fun buildExtendedParallelModularAutomaton(
    context: Context,
    model: Model,
): ParallelModularAutomaton {
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val modularContext: ModularContext = context["modularContext"]
    val moduleControllingOutputVariable = context.convertIntVarArray("moduleControllingOutputVariable", model)

    val modules = modularContext.mapIndexed { (m), ctx ->
        val C: Int = ctx["C"]
        val K: Int = ctx["K"]
        val P: Int = ctx["P"]
        val Z: Int = ctx["Z"]
        val transitionDestination = ctx.convertIntVarArray("transitionDestination", model)
        val transitionInputEvent = ctx.convertIntVarArray("transitionInputEvent", model)
        val stateOutputEvent = ctx.convertIntVarArray("stateOutputEvent", model)
        val stateAlgorithmBot = ctx.convertBoolVarArray("stateAlgorithmBot", model)
        val stateAlgorithmTop = ctx.convertBoolVarArray("stateAlgorithmTop", model)
        val nodeType = ctx.convertDomainVarArray<NodeType>("nodeType", model)
        val nodeInputVariable = ctx.convertIntVarArray("nodeInputVariable", model)
        val nodeParent = ctx.convertIntVarArray("nodeParent", model)
        val nodeChild = ctx.convertIntVarArray("nodeChild", model)
        val moduleOutputVariables = (1..Z).filter { z -> moduleControllingOutputVariable[z] == m }

        Automaton(
            scenarioTree.inputEvents,
            scenarioTree.outputEvents,
            scenarioTree.inputNames,
            moduleOutputVariables.map { z -> scenarioTree.outputNames[z - 1] }
        ).endow(
            C = C, K = K,
            stateOutputEvent = { c ->
                stateOutputEvent[c].let { o ->
                    if (o == 0) null
                    else scenarioTree.outputEvents[o - 1]
                }
            },
            stateAlgorithm = { c ->
                BinaryAlgorithm(
                    algorithm0 = moduleOutputVariables.map { z -> stateAlgorithmBot[c, z] },
                    algorithm1 = moduleOutputVariables.map { z -> stateAlgorithmTop[c, z] }
                )
            },
            transitionDestination = { c, k ->
                transitionDestination[c, k]
            },
            transitionInputEvent = { c, k ->
                scenarioTree.inputEvents[transitionInputEvent[c, k] - 1]
            },
            transitionGuard = { c, k ->
                BooleanExpressionGuard.from(
                    nodeType = MultiArray.new(P) { (p) -> nodeType[c, k, p] },
                    terminal = IntMultiArray.new(P) { (p) -> nodeInputVariable[c, k, p] },
                    parent = IntMultiArray.new(P) { (p) -> nodeParent[c, k, p] },
                    childLeft = IntMultiArray.new(P) { (p) -> nodeChild[c, k, p] },
                    childRight = IntMultiArray.new(P) { (p) ->
                        if (nodeType[c, k, p] in setOf(NodeType.AND, NodeType.OR))
                            nodeChild[c, k, p] + 1
                        else
                            0
                    },
                    inputNames = scenarioTree.inputNames
                )
            }
        )
    }

    return ParallelModularAutomaton(modules, moduleControllingOutputVariable)
}
