package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.MutableMultiArray
import com.github.lipen.multiarray.map
import com.github.lipen.multiarray.mapToMut
import com.github.lipen.satlib.core.Context
import com.github.lipen.satlib.core.Model
import com.soywiz.klock.DateTime
import okio.buffer
import okio.sink
import org.redundent.kotlin.xml.xml
import ru.ifmo.fbsat.core.automaton.guard.BooleanExpressionGuard
import ru.ifmo.fbsat.core.automaton.guard.DnfGuard
import ru.ifmo.fbsat.core.automaton.guard.NodeType
import ru.ifmo.fbsat.core.automaton.guard.TruthTableGuard
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.initialOutputValues
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.convertBoolVarArray
import ru.ifmo.fbsat.core.solver.convertDomainVarArray
import ru.ifmo.fbsat.core.solver.convertIntVarArray
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.Pins
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.ModularContext
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.pow
import ru.ifmo.fbsat.core.utils.random
import ru.ifmo.fbsat.core.utils.toBinary
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

private val logger = MyLogger {}

class ArbitraryModularAutomaton(
    val modules: MultiArray<Automaton>,
    val inboundVarPinParent: MultiArray<Int>,
    val inputEvents: List<InputEvent>,
    val outputEvents: List<OutputEvent>,
    val inputNames: List<String>,
    val outputNames: List<String>,
    val initialOutputValues: OutputValues = OutputValues.zeros(outputNames.size),
) {
    /** Number of modules */
    @Suppress("PropertyName")
    val M: Int = modules.shape.single()
    val numberOfModules: Int = M
    val numberOfStates: Int = modules.values.sumOf { it.numberOfStates }
    val numberOfTransitions: Int = modules.values.sumOf { it.numberOfTransitions }
    val totalGuardsSize: Int = modules.values.sumOf { it.totalGuardsSize }

    constructor(
        modules: MultiArray<Automaton>,
        inboundVarPinParent: MultiArray<Int>,
        scenarioTree: PositiveScenarioTree,
    ) : this(
        modules = modules,
        inboundVarPinParent = inboundVarPinParent,
        inputEvents = scenarioTree.inputEvents,
        outputEvents = scenarioTree.outputEvents,
        inputNames = scenarioTree.inputNames,
        outputNames = scenarioTree.outputNames,
        initialOutputValues = scenarioTree.initialOutputValues,
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
        val modularOutputAction: MultiArray<OutputAction>,
    )

    @Suppress("LocalVariableName")
    fun eval(inputActions: Sequence<InputAction>): Sequence<EvalResult> {
        val E = inputEvents.size
        val O = outputEvents.size
        val X = inputNames.size
        val Z = outputNames.size

        with(Pins(M = M, X = X, Z = Z, E = E, O = O)) {
            val currentModularState = modules.mapToMut { it.initialState }
            val currentModularInputAction = modules.mapToMut {
                InputAction(
                    null,
                    InputValues.zeros(it.inputNames.size)
                )
            }
            val currentModularOutputValues = modules.mapToMut { it.initialOutputValues }
            val currentModularOutputEvent: MutableMultiArray<OutputEvent?> = modules.mapToMut { null }
            val currentInboundVarPinComputedValue = MutableMultiArray.newBoolean(allInboundVarPins.size)
            val currentOutboundVarPinComputedValue = MutableMultiArray.newBoolean(allOutboundVarPins.size)
            var currentOutputValues = initialOutputValues

            return inputActions.map { inputAction ->
                // update external outbound var (input var) pins
                for (x in 1..X) {
                    val pin = externalOutboundVarPins[x - 1]
                    currentOutboundVarPinComputedValue[pin] = inputAction.values[x - 1]
                }

                for (m in 1..M) {
                    // update modular inbound pins
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
                    val moduleInputAction = InputAction(moduleInputEvent, moduleInputValues)
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

                    // update modular outbound pins
                    for (z in 1..Z) {
                        if (z <= modules[m].outputNames.size) {
                            val pin = modularOutboundVarPins[m][z - 1]
                            currentOutboundVarPinComputedValue[pin] = currentModularOutputValues[m][z - 1]
                        }
                    }
                }

                // update external inbound var (output var) pins
                for (pin in externalInboundVarPins) {
                    val parent = inboundVarPinParent[pin]
                    if (parent != 0) {
                        currentInboundVarPinComputedValue[pin] = currentOutboundVarPinComputedValue[parent]
                    }
                }

                // merge output events
                val outputEvent = currentModularOutputEvent[M]

                // merge output values (composite)
                if (outputEvent != null) {
                    currentOutputValues = OutputValues(
                        externalInboundVarPins.map { currentInboundVarPinComputedValue[it] }
                    )
                }

                EvalResult(
                    inputAction = inputAction,
                    outputAction = OutputAction(outputEvent, currentOutputValues),
                    modularInputAction = currentModularInputAction.map { it },
                    modularDestination = currentModularState.map { it },
                    modularOutputAction = MultiArray.new(M) { (m) ->
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
                logger.error("i = $i: FAILED")
                logger.error("Bad output action: result.outputAction != element.outputAction")
                logger.error("result.outputAction = ${result.outputAction}")
                logger.error("element.outputAction = ${element.outputAction}")
                logger.error("element = $element")
                logger.error("result.modularInputAction = ${result.modularInputAction.values.map { "${it.event}[${it.values.values.toBinaryString()}]" }}")
                logger.error("result.modularDestination = ${result.modularDestination.values.map { it.id }}")
                logger.error("result.modularOutputAction = ${result.modularOutputAction.values.map { "${it.event}[${it.values.values.toBinaryString()}]" }}")
                return false
            }
            i++
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
        logger.info("    " + getStats())
    }

    fun dump(dir: File, name: String = "automaton") {
        logger.info("Dumping '$name' to <$dir>...")
        dir.mkdirs()
        dumpGv(dir)
        dumpFbt(dir.resolve("$name.fbt"), name = name)
    }

    fun dumpGv(dir: File) {
        for (m in 1..M) {
            modules[m].dumpGv(dir.resolve("module_$m.gv"))
        }
        // TODO: dump compound gv
    }

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

        with(Pins(M = M, X = X, Z = Z, E = E, O = O)) {
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

    val modularEffectiveInputs = MultiArray.new(M) { mutableListOf<InputValues>() }
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
        logger.info("Module #$m effective inputs (${modularEffectiveInputs[m].size} total):")
        for (input in modularEffectiveInputs[m]) {
            logger.info("    ${input.values.toBinaryString()}")
        }
    }

    println("=======================")

    for (m in 1..M) with(modules[m]) {
        logger.info("Minimizing guards for module #$m...")
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
        logger.debug { "Writing PLA to '$inputFile'..." }
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
        logger.debug { "Done writing PLA to '$inputFile'" }

        // val command = "boom -SD -Si100 $inputFile"
        val command = "espresso $inputFile"
        logger.debug { "Executing '$command'..." }
        val process = Runtime.getRuntime().exec(command)

        val guardProducts: List<MutableList<List<String>>> = List(T) { mutableListOf() }
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
            logger.debug { "Minimized guard: ${transition.guard.toSimpleString()}" }
        }
    }
}

@Suppress("LocalVariableName")
fun buildBasicArbitraryModularAutomaton(
    context: Context,
    model: Model,
): ArbitraryModularAutomaton {
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val modularContext: ModularContext = context["modularContext"]
    val inboundVarPinParent = context.convertIntVarArray("inboundVarPinParent", model)

    val modules = modularContext.map { ctx ->
        val C: Int = ctx["C"]
        val K: Int = ctx["K"]
        val X: Int = ctx["X"]
        val Z: Int = ctx["Z"]
        val U: Int = ctx["U"]
        val transitionDestination = ctx.convertIntVarArray("transitionDestination", model)
        val firstFired = ctx.convertIntVarArray("firstFired", model)
        val notFired = ctx.convertBoolVarArray("notFired", model)
        val stateAlgorithmBot = ctx.convertBoolVarArray("stateAlgorithmBot", model)
        val stateAlgorithmTop = ctx.convertBoolVarArray("stateAlgorithmTop", model)
        val initialOutputValues: OutputValues = ctx["initialOutputValues"]

        Automaton(
            scenarioTree = scenarioTree,
            initialOutputValues = initialOutputValues
        ).endow(
            C = C, K = K,
            stateOutputEvent = { OutputEvent("CNF") },
            stateAlgorithm = { c ->
                BinaryAlgorithm(
                    algorithm0 = BooleanArray(Z) { z0 -> stateAlgorithmBot[c, z0 + 1] },
                    algorithm1 = BooleanArray(Z) { z0 -> stateAlgorithmTop[c, z0 + 1] }
                )
            },
            transitionDestination = { c, k -> transitionDestination[c, k] },
            transitionInputEvent = { _, _ -> InputEvent("REQ") },
            transitionGuard = { c, k ->
                TruthTableGuard(
                    truthTable = (1..U).associate { u ->
                        InputValues(
                            (u - 1).toString(2).padStart(X, '0').reversed().map {
                                when (it) {
                                    '1' -> true
                                    '0' -> false
                                    else -> error("Bad bit '$it'")
                                }
                            }
                        ) to when {
                            notFired[c, k, 1, u] -> false
                            firstFired[c, 1, u] == k -> true
                            else -> null
                        }
                    }
                    // inputNames = scenarioTree.inputNames,
                    // uniqueInputs = scenarioTree.uniqueInputs
                )
            }
        )
    }

    return ArbitraryModularAutomaton(
        modules = modules,
        inboundVarPinParent = inboundVarPinParent,
        scenarioTree = scenarioTree,
    )
}

fun buildExtendedArbitraryModularAutomaton(
    context: Context,
    model: Model,
): ArbitraryModularAutomaton {
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val modularContext: ModularContext = context["modularContext"]
    val inboundVarPinParent = context.convertIntVarArray("inboundVarPinParent", model)

    val modules = modularContext.map { ctx ->
        val C: Int = ctx["C"]
        val K: Int = ctx["K"]
        val P: Int = ctx["P"]
        val X: Int = ctx["X"]
        val Z: Int = ctx["Z"]
        val U: Int = ctx["U"]
        val transitionDestination = ctx.convertIntVarArray("transitionDestination", model)
        val firstFired = ctx.convertIntVarArray("firstFired", model)
        val notFired = ctx.convertBoolVarArray("notFired", model)
        val stateAlgorithmBot = ctx.convertBoolVarArray("stateAlgorithmBot", model)
        val stateAlgorithmTop = ctx.convertBoolVarArray("stateAlgorithmTop", model)
        val nodeType = ctx.convertDomainVarArray<NodeType>("nodeType", model)
        val nodeInputVariable = ctx.convertIntVarArray("nodeInputVariable", model)
        val nodeParent = ctx.convertIntVarArray("nodeParent", model)
        val nodeChild = ctx.convertIntVarArray("nodeChild", model)
        val initialOutputValues: OutputValues = ctx["initialOutputValues"]

        Automaton(
            scenarioTree = scenarioTree,
            initialOutputValues = initialOutputValues
        ).endow(
            C = C, K = K,
            stateOutputEvent = { OutputEvent("CNF") },
            stateAlgorithm = { c ->
                BinaryAlgorithm(
                    algorithm0 = BooleanArray(Z) { z0 -> stateAlgorithmBot[c, z0 + 1] },
                    algorithm1 = BooleanArray(Z) { z0 -> stateAlgorithmTop[c, z0 + 1] }
                )
            },
            transitionDestination = { c, k -> transitionDestination[c, k] },
            transitionInputEvent = { _, _ -> InputEvent("REQ") },
            transitionGuard = { c, k ->
                BooleanExpressionGuard.from(
                    nodeType = MultiArray.new(P) { (p) -> nodeType[c, k, p] },
                    terminal = MultiArray.new(P) { (p) -> nodeInputVariable[c, k, p] },
                    parent = MultiArray.new(P) { (p) -> nodeParent[c, k, p] },
                    childLeft = MultiArray.new(P) { (p) -> nodeChild[c, k, p] },
                    childRight = MultiArray.new(P) { (p) ->
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

    return ArbitraryModularAutomaton(
        modules = modules,
        inboundVarPinParent = inboundVarPinParent,
        scenarioTree = scenarioTree,
    )
}
