package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import com.soywiz.klock.DateTime
import okio.buffer
import okio.sink
import org.redundent.kotlin.xml.xml
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.Pins
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.pow
import ru.ifmo.fbsat.core.utils.random
import ru.ifmo.fbsat.core.utils.toBinary
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.writeln
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
    val M: Int = modules.shape.single()
    val numberOfTransitions: Int = modules.values.sumBy { it.numberOfTransitions }
    val totalGuardsSize: Int = modules.values.sumBy { it.totalGuardsSize }

    constructor(
        modules: MultiArray<Automaton>,
        inboundVarPinParent: MultiArray<Int>,
        scenarioTree: OldPositiveScenarioTree
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
        val modularInputAction: MultiArray<InputAction>,
        val modularDestination: MultiArray<Automaton.State>,
        val modularOutputAction: MultiArray<OutputAction>
    )

    @Suppress("LocalVariableName")
    fun eval(inputActions: Sequence<InputAction>): Sequence<EvalResult> {
        val E = inputEvents.size
        val O = outputEvents.size
        val X = inputNames.size
        val Z = outputNames.size

        with(Pins(M, X, Z, E, O)) {
            val currentModularState = modules.map { it.initialState }
            val currentModularInputAction = modules.map {
                InputAction(
                    null,
                    InputValues.zeros(it.inputNames.size)
                )
            }
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
                    val moduleInputEvent: InputEvent? =
                        if (m == 1) inputAction.event
                        else currentModularOutputEvent[m - 1]?.let {
                            check(it.name == "CNF")
                            InputEvent("REQ")
                        }
                    val moduleInputValues = InputValues(
                        modularInboundVarPins[m].map { currentInboundVarPinComputedValue[it] }
                    )
                    val moduleInputAction =
                        InputAction(moduleInputEvent, moduleInputValues)
                    val result = modules[m].eval(
                        inputAction = moduleInputAction,
                        state = currentModularState[m],
                        values = currentModularOutputValues[m]
                    )

                    // update current state etc
                    currentModularState[m] = result.destination
                    currentModularInputAction[m] = moduleInputAction
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
                    currentModularInputAction.map { it },
                    currentModularState.map { it },
                    MultiArray.create(M) { (m) ->
                        OutputAction(
                            currentModularOutputEvent[m],
                            currentModularOutputValues[m]
                        )
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
                log.error("result.modularInputAction = ${result.modularInputAction.values.map { "${it.event}[${it.values.values.toBinaryString()}]" }}")
                log.error("result.modularDestination = ${result.modularDestination.values.map { it.id }}")
                log.error("result.modularOutputAction = ${result.modularOutputAction.values.map { "${it.event}[${it.values.values.toBinaryString()}]" }}")
                return false
            } else {
                log.success("i = $i: OK")
            }
            i++
        }
        return true
    }

    fun verify(scenarioTree: OldPositiveScenarioTree): Boolean =
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

        with(Pins(M, X, Z, E, O)) {
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
                            for ((x0, moduleInputName) in inputNames.withIndex()) {
                                val pin = modularInboundVarPins[m][x0]
                                when (val parent = inboundVarPinParent[pin]) {
                                    0 -> {
                                        // Do nothing
                                    }
                                    in externalOutboundVarPins -> {
                                        val extx0 = externalOutboundVarPins.indexOf(parent)
                                        val externalInputName = inputNames[extx0]
                                        "Connection"(
                                            "Source" to externalInputName,
                                            "Destination" to "M$m.$moduleInputName"
                                        )
                                    }
                                    else -> {
                                        val mp = (parent - 1) / Z + 1
                                        val zp0 = modularOutboundVarPins[mp].indexOf(parent)
                                        val parentModuleOutputName = outputNames[zp0]
                                        "Connection"(
                                            "Source" to "M$mp.$parentModuleOutputName",
                                            "Destination" to "M$m.$moduleInputName"
                                        )
                                    }
                                }
                            }
                        }
                        for ((z0, externalOutputName) in outputNames.withIndex()) {
                            val pin = externalInboundVarPins[z0]
                            when (val parent = inboundVarPinParent[pin]) {
                                0 -> {
                                    // Do nothing
                                }
                                in externalOutboundVarPins -> {
                                    val extx0 = externalOutboundVarPins.indexOf(parent)
                                    val externalInputName = inputNames[extx0]
                                    "Connection"(
                                        "Source" to externalInputName,
                                        "Destination" to externalOutputName
                                    )
                                }
                                else -> {
                                    val mp = (parent - 1) / Z + 1
                                    val zp0 = modularOutboundVarPins[mp].indexOf(parent)
                                    val parentModuleOutputName = outputNames[zp0]
                                    "Connection"(
                                        "Source" to "M$mp.$parentModuleOutputName",
                                        "Destination" to externalOutputName
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

fun ArbitraryModularAutomaton.minimizeTruthTableGuards(scenarioTree: OldPositiveScenarioTree) {
    println("=======================")

    val modularEffectiveInputs = MultiArray.create(M) { mutableListOf<InputValues>() }
    for (scenario in scenarioTree.scenarios) {
        val elements = scenario.elements.asSequence()
        val results = eval(elements.map { it.inputAction })
        for (result in results) {
            for (m in 1..M) {
                modularEffectiveInputs[m].add(result.modularInputAction[m].values)
            }
        }
    }

    for (m in 1..M) {
        modularEffectiveInputs[m].sortBy { it.values.toBinaryString() }
        log.info("Module #$m effective inputs (${modularEffectiveInputs[m].size} total):")
        for (input in modularEffectiveInputs[m]) {
            log.info("    ${input.values.toBinaryString()}")
        }
    }

    println("=======================")

    for (m in 1..M) with(modules[m]) {
        log.info("Minimizing guards for module #$m...")
        val T = numberOfTransitions
        val X = inputNames.size
        val U = 2.pow(X)
        val allInputValues: List<InputValues> = (1..U).map { u ->
            InputValues(
                (u - 1).toBinary(X)
            )
        }
        val moduleInputValues: List<InputValues> = modularEffectiveInputs[m]

        val inputFile = File("pla-m$m.in")
        log.debug { "Writing PLA to '$inputFile'..." }
        inputFile.sink().buffer().useWith {
            writeln(".i $X")
            writeln(".o $T")
            writeln(".ilb " + inputNames.joinToString(" "))
            // TODO: .ob <output_names...>
            // Boom requires some 'type' (and seems to support only "fr")
            // Espresso does not require 'type' at all
            writeln(".type fr")
            for (input in moduleInputValues) {
                val evaluation = transitions.map {
                    (it.guard as TruthTableGuard).truthTable[input]
                }
                val s = evaluation.map {
                    when (it) {
                        true -> '1'
                        false -> '0'
                        null -> '-'
                    }
                }.joinToString("")
                writeln("${input.values.toBinaryString()} $s")
            }
            for (input in allInputValues - moduleInputValues) {
                writeln("${input.values.toBinaryString()} " + "-".repeat(T))
            }
            writeln(".e")
        }
        log.debug { "Done writing PLA to '$inputFile'" }

        // val command = "boom -SD -Si100 $inputFile"
        val command = "espresso $inputFile"
        log.debug { "Executing '$command'..." }
        val process = Runtime.getRuntime().exec(command)

        val guardProducts: List<MutableList<List<String>>> = List(T) { mutableListOf<List<String>>() }
        process.inputStream.bufferedReader().useLines { lines ->
            for (line in lines) {
                if (line.startsWith('.')) continue

                val (productString, guardsString) = line.split(' ', limit = 2)
                for ((i, c) in guardsString.withIndex()) {
                    if (c == '1') {
                        val literals = productString.mapIndexedNotNull { i, c ->
                            when (c) {
                                '0' -> "~" + inputNames[i]
                                '1' -> inputNames[i]
                                else -> null
                            }
                        }
                        guardProducts[i].add(literals)
                    }
                }
            }
        }
        process.destroy()

        for ((i, transition) in transitions.withIndex()) {
            val products: List<List<String>> = guardProducts[i]
            // val dnf = makeDnfString(products)
            // log.debug { "Minimized guard: $dnf" }
            transition.guard = DnfGuard(products, inputNames)
            log.debug { "Minimized guard: ${transition.guard.toSimpleString()}" }
        }
    }
}
