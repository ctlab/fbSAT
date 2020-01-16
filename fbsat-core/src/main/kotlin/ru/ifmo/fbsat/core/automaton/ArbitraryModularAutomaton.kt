package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import com.soywiz.klock.DateTime
import org.redundent.kotlin.xml.xml
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.PinVars
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.random
import java.io.File

class ArbitraryModularAutomaton(
    val modules: MultiArray<Automaton>,
    val inboundVarPinParent: MultiArray<Int>,
    val inputEvents: List<InputEvent>,
    val outputEvents: List<OutputEvent>,
    val inputNames: List<String>,
    val outputNames: List<String>
) {
    /** Number of modules */
    @Suppress("PropertyName")
    val M: Int = modules.shape[0]
    val numberOfTransitions: Int = modules.values.sumBy { it.numberOfTransitions }
    val totalGuardsSize: Int = modules.values.sumBy { it.totalGuardsSize }

    constructor(
        modules: MultiArray<Automaton>,
        inboundVarPinParent: MultiArray<Int>,
        scenarioTree: ScenarioTree
    ) : this(
        modules,
        inboundVarPinParent,
        scenarioTree.inputEvents,
        scenarioTree.outputEvents,
        scenarioTree.inputNames,
        scenarioTree.outputNames
    )

    init {
        require(M >= 2)
    }

    // fun eval(
    //     inputAction: InputAction,
    //     modularState: MultiArray<Automaton.State>,
    //     modularOutputValues: MultiArray<OutputValues>
    // ): MultiArray<Automaton.EvalResult> {
    //     TODO()
    // }

    data class EvalResult(
        val inputAction: InputAction,
        val outputAction: OutputAction,
        val modularDestination: MultiArray<Automaton.State>,
        val modularOutputAction: MultiArray<OutputAction>
    )

    @Suppress("LocalVariableName")
    fun eval(inputActions: Sequence<InputAction>): Sequence<EvalResult> {
        val E = inputEvents.size
        val O = outputEvents.size
        val X = inputNames.size
        val Z = outputNames.size

        with(PinVars(M, X, Z, E, O)) {
            val currentModularState = modules.map { it.initialState }
            val currentModularOutputValues = modules.map { OutputValues.zeros(it.outputNames.size) }
            val currentModularOutputEvent: MultiArray<OutputEvent?> = modules.map { null }
            val currentInboundVarPinComputedValue = BooleanMultiArray.create(allInboundVarPins.size)
            val currentOutboundVarPinComputedValue = BooleanMultiArray.create(allOutboundVarPins.size)
            var currentOutputValues = OutputValues.zeros(Z)

            return inputActions.map { inputAction ->
                for (x in 1..X) {
                    val pin = externalOutboundVarPins[x - 1]
                    currentOutboundVarPinComputedValue[pin] = inputAction.values[x - 1]
                }

                for (m in 1..M) {
                    // update inbound pins
                    for (pin in modularInboundVarPins[m]) {
                        val parent = inboundVarPinParent[pin]
                        if (parent != 0) {
                            currentInboundVarPinComputedValue[pin] = currentOutboundVarPinComputedValue[parent]
                        }
                    }

                    // eval module
                    val inputValues = InputValues(
                        modularInboundVarPins[m].map { currentInboundVarPinComputedValue[it] }
                    )
                    val result =
                        if (m == 1 || currentModularOutputEvent[m - 1] != null)
                            modules[m].eval(
                                inputAction = InputAction(InputEvent("REQ"), inputValues),
                                state = currentModularState[m],
                                values = currentModularOutputValues[m]
                            )
                        else
                            Automaton.EvalResult(
                                currentModularState[m],
                                OutputAction(null, currentModularOutputValues[m])
                            )

                    // save new state and output values (modular)
                    currentModularState[m] = result.destination
                    currentModularOutputEvent[m] = result.outputAction.event
                    currentModularOutputValues[m] = result.outputAction.values

                    // update outbound pins
                    for (z in 1..Z) {
                        val pin = modularOutboundVarPins[m][z - 1]
                        currentOutboundVarPinComputedValue[pin] = currentModularOutputValues[m][z - 1]
                    }
                }

                // update external inbound var pins
                for (pin in externalInboundVarPins) {
                    val parent = inboundVarPinParent[pin]
                    if (parent != 0) {
                        currentInboundVarPinComputedValue[pin] = currentOutboundVarPinComputedValue[parent]
                    }
                }

                // merge output events
                val outputEvent = currentModularOutputEvent[M]

                // merge output values (composite)
                if (outputEvent != null)
                    currentOutputValues = OutputValues(
                        externalInboundVarPins.map { currentInboundVarPinComputedValue[it] }
                    )

                EvalResult(
                    inputAction,
                    OutputAction(outputEvent, currentOutputValues),
                    currentModularState.map { it },
                    MultiArray.create(M) { (m) ->
                        OutputAction(currentModularOutputEvent[m], currentModularOutputValues[m])
                    }
                )
            }
        }
    }

    @Suppress("LocalVariableName")
    fun verify(scenario: PositiveScenario): Boolean {
        val elements = scenario.elements.asSequence()
        var i = 1
        for ((element, result) in elements.zip(eval(elements.map { it.inputAction }))) {
            if (result.outputAction != element.outputAction) {
                log.error("i = $i: FAILED")
                log.error("Bad output action: result.outputAction != element.outputAction")
                log.error("result.outputAction = ${result.outputAction}")
                log.error("element.outputAction = ${element.outputAction}")
                log.error("element = $element")
                log.error("result.modularDestination = ${result.modularDestination.map { it.id }}")
                log.error("result.modularOutputAction = ${result.modularOutputAction.values}")
                return false
            } else {
                log.success("i = $i: OK")
            }
            i++
        }
        return true
    }

    fun verify(scenarioTree: ScenarioTree): Boolean =
        scenarioTree.scenarios.all(::verify)

    /**
     * Dump modular automaton in FBT format to [file].
     */
    fun dumpFbt(file: File, name: String? = null) {
        for (m in 1..M) {
            modules[m].dumpFbt(file.resolveSibling("module_$m.fbt"), "module_$m")
        }
        file.writeText(toFbtString(name))
    }

    /**
     * Stringify automaton to FBT format.
     */
    fun toFbtString(name: String? = null): String {
        fun r() = "%.3f".format((1.0..1000.0).random())

        val E = inputEvents.size
        val O = outputEvents.size
        val X = inputNames.size
        val Z = outputNames.size

        with(PinVars(M, X, Z, E, O)) {
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
                    "EventConnections" {
                        check(inputEvents.single().name == "REQ")
                        check(outputEvents.single().name == "CNF")
                        "Connection"(
                            "Source" to "REQ",
                            "Destination" to "M1.REQ"
                        )
                        for (m in 2..M) {
                            "Connection"(
                                "Source" to "M${m - 1}.CNF",
                                "Destination" to "M$m.REQ"
                            )
                        }
                        "Connection"(
                            "Source" to "M$M.CNF",
                            "Destination" to "CNF"
                        )
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
                        for (m in 1..M) {
                            for ((x0, inputName) in inputNames.withIndex()) {
                                val pin = modularInboundVarPins[m][x0]
                                when (val parent = inboundVarPinParent[pin]) {
                                    0 -> {
                                        // Do nothing
                                    }
                                    in externalOutboundVarPins -> {
                                        val extx0 = externalOutboundVarPins.indexOf(parent)
                                        val extInputName = inputNames[extx0]
                                        "Connection"(
                                            "Source" to extInputName,
                                            "Destination" to "M$m.$inputName"
                                        )
                                    }
                                    else -> {
                                        val mp = (parent - 1) / Z + 1
                                        val zp0 = modularOutboundVarPins[mp].indexOf(parent)
                                        val outputName = outputNames[zp0]
                                        "Connection"(
                                            "Source" to "M$mp.$outputName",
                                            "Destination" to "M$m.$inputName"
                                        )
                                    }
                                }
                            }
                        }
                        for ((z0, outputName) in outputNames.withIndex()) {
                            val pin = externalInboundVarPins[z0]
                            when (val parent = inboundVarPinParent[pin]) {
                                0 -> {
                                    // Do nothing
                                }
                                in externalOutboundVarPins -> {
                                    val extx0 = externalOutboundVarPins.indexOf(parent)
                                    val extInputName = inputNames[extx0]
                                    "Connection"(
                                        "Source" to extInputName,
                                        "Destination" to outputName
                                    )
                                }
                                else -> {
                                    val mp = (parent - 1) / Z + 1
                                    val zp0 = modularOutboundVarPins[mp].indexOf(parent)
                                    val outputName = outputNames[zp0]
                                    "Connection"(
                                        "Source" to "M$mp.$outputName",
                                        "Destination" to outputName
                                    )
                                }
                            }
                        }
                    }
                }
            }.toString(Globals.xmlPrintOptions)
        }
    }
}
