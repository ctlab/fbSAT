@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.cli.entry

import com.github.lipen.multiarray.MultiArray
import com.soywiz.klock.PerformanceCounter
import nl.adaptivity.xmlutil.serialization.XML
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenario
import ru.ifmo.fbsat.core.scenario.negative.THE_Counterexample
import ru.ifmo.fbsat.core.scenario.negative.readCounterexampleFromFile
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.toOld
import ru.ifmo.fbsat.core.solver.MiniSat
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.distributed.complete.distributedCegis
import ru.ifmo.fbsat.core.utils.EpsilonOutputEvents
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.createNullable
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.project
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.toMultiArray
import java.io.File

fun main() {
    val timeStart = PerformanceCounter.reference

    val M = 2
    // 1: sender
    // 2: receiver
    val modularName = MultiArray.create(M) { (m) ->
        when (m) {
            1 -> "sndr"
            2 -> "rcvr"
            else -> error("Are you lost?")
        }
    }

    val inputMapping: Map<String, String> = mapOf(
        "sndr.send" to "sclt.send",
        "sndr.timeout" to "tmr.timeout",
        "sndr.acknowledge" to "bwdc.output",
        "sndr.input_bit" to "bwdc.output_bit",
        "rcvr.packet" to "fwdc.output",
        "rcvr.input_bit" to "fwdc.output_bit"
    )

    val modularInputEvents = MultiArray.create(M) { (m) ->
        when (m) {
            1 -> listOf("send", "timeout", "acknowledge")
            2 -> listOf("packet")
            else -> error("Are you lost?")
        }
            // .map { "${modularName[m]}.$it" }
            .map { InputEvent(it) }
    }
    val modularOutputEvents = MultiArray.create(M) { (m) ->
        when (m) {
            1 -> listOf("done", "packet")
            2 -> listOf("deliver", "acknowledge")
            else -> error("Are you lost?")
        }
            // .map { "${modularName[m]}.$it" }
            .map { OutputEvent(it) }
    }
    val modularInputNames = MultiArray.create(M) { (m) ->
        when (m) {
            1 -> listOf("input_bit")
            2 -> listOf("input_bit")
            else -> error("Are you lost?")
        } // .map { "${modularName[m]}.$it" }
    }
    val modularOutputNames = MultiArray.create(M) { (m) ->
        when (m) {
            1 -> listOf("output_bit")
            2 -> listOf("output_bit")
            else -> error("Are you lost?")
        } // .map { "${modularName[m]}.$it" }
    }
    val solver = MiniSat()
    val outDir = File("out/abp-take")
    val inferrer = Inferrer(solver, outDir)

    check(modularOutputNames.values.all { it.size == 1 })
    Globals.INITIAL_OUTPUT_VALUES = OutputValues.zeros(1)
    Globals.IS_FORBID_TRANSITIONS_TO_FIRST_STATE = false
    Globals.EPSILON_OUTPUT_EVENTS = EpsilonOutputEvents.NONE
    // Globals.IS_BFS_AUTOMATON = false
    // Globals.IS_BFS_GUARD = false
    Globals.IS_DEBUG = true
    Globals.modularName = listOf("sender", "receiver").toMultiArray()

    val xml = XML {
        unknownChildHandler = { input, isAttribute, name, candidates ->
            println("unknown child, whatever this means!")
            println("  - input: $input")
            println("  - isAttribute: $isAttribute")
            println("  - name: $name")
            println("  - candidates: $candidates")
        }
    }
    val sXml = File("data/abp-take/trace.xml").readText().replace("<loops> </loops>", "<loops/>")
    val trace = xml.parse(THE_Counterexample.serializer(), sXml)
    // println("trace = "); pp(trace)
    // val states: List<Map<String, Boolean>> = trace.nodes.map { node ->
    //     val state = node.states.single()
    //     state.values.associate { value ->
    //         value.variable to value.content.toBoolean()
    //     }
    // }
    // print("states[0] = "); pp(states[0])

    val traceElements = trace.nodes
        .map { node ->
            node.states.single().values.associate { value ->
                value.variable to value.content.toBoolean()
            }
        }
        .zipWithNext { inputData, outputData ->
            CompoundScenarioElement(
                modular = MultiArray.create(M) { (m) ->
                    ScenarioElement(
                        inputAction = InputAction(
                            event = modularInputEvents[m].firstOrNull { inputData.getValue(inputMapping.getValue("${modularName[m]}.${it.name}")) },
                            values = InputValues(modularInputNames[m].map { inputData.getValue(inputMapping.getValue("${modularName[m]}.$it")) })
                        ),
                        outputAction = OutputAction(
                            event = modularOutputEvents[m].firstOrNull { outputData.getValue("${modularName[m]}.${it.name}") },
                            values = OutputValues(modularOutputNames[m].map { outputData.getValue("${modularName[m]}.$it") })
                        )
                    )
                }
            )
        }
        .filter { element ->
            element.modularInputEvent.values.all { it != null }
        }
    // for (i in elements.indices) {
    //     if (elements[i].modularInputAction.values.any { it.event == null }) {
    //         log.warn("null input event in element i = $i: ${elements[i]}")
    //     }
    // }
    val positiveCompoundScenario = PositiveCompoundScenario(M, traceElements)
    // println("scenario = $scenario")

    val positiveCompoundScenarioTree = PositiveCompoundScenarioTree(
        M = M,
        modularInputEvents = modularInputEvents,
        modularOutputEvents = modularOutputEvents,
        modularInputNames = modularInputNames,
        modularOutputNames = modularOutputNames
    )
    positiveCompoundScenarioTree.addScenario(positiveCompoundScenario)

    val oldTree = positiveCompoundScenarioTree.modular[1].toOld()
    val oldScenario = oldTree.scenarios[0]
    println("oldScenario.elements:")
    for (element in oldScenario.elements) {
        println("  - $element")
    }

    println("positiveCompoundScenarioTree.modular[1].uniqueInputs: ${positiveCompoundScenarioTree.modular[1].uniqueInputs}")
    println("oldTree.uniqueInputs: ${oldTree.uniqueInputs}")

    // ===== Counterexample

    val counterexamples = readCounterexampleFromFile(File("data/abp-take/ce.xml"))
    val counterexample = counterexamples.first()
    // val counterexample = xml.parse(THE_Counterexample.serializer(), xmlString)
    val loopPosition = counterexample.loops.trim().split(" ").firstOrNull()?.toInt()
    println("loopPosition = $loopPosition")
    val ceElements = counterexample.nodes
        .map { node ->
            node.states.single().values.associate { value ->
                value.variable to value.content.toBoolean()
            }
        }
        .zipWithNext { inputData, outputData ->
            CompoundScenarioElement(
                modular = MultiArray.create(M) { (m) ->
                    ScenarioElement(
                        inputAction = InputAction(
                            event = modularInputEvents[m].firstOrNull { inputData.getValue(inputMapping.getValue("${modularName[m]}.${it.name}")) },
                            values = InputValues(modularInputNames[m].map { inputData.getValue(inputMapping.getValue("${modularName[m]}.$it")) })
                        ),
                        outputAction = OutputAction(
                            event = modularOutputEvents[m].firstOrNull { outputData.getValue("${modularName[m]}.${it.name}") },
                            values = OutputValues(modularOutputNames[m].map { outputData.getValue("${modularName[m]}.$it") })
                        )
                    )
                }
            )
        }
    // .filter { element ->
    //     element.modularInputEvent.values.all { it != null }
    // }
    val negativeScenario = NegativeCompoundScenario(M, ceElements, loopPosition?.let { it - 1 })
    val negativeCompoundScenarioTree = NegativeCompoundScenarioTree(
        M = 1,
        modularInputEvents = modularInputEvents,
        modularOutputEvents = modularOutputEvents,
        modularInputNames = modularInputNames,
        modularOutputNames = modularOutputNames
    )
    negativeCompoundScenarioTree.addScenario(negativeScenario)

    println("negativeCompoundScenarioTree.modular[1].uniqueInputs: ${negativeCompoundScenarioTree.modular[1].uniqueInputs}")

    val negativeScenario1 = NegativeScenario.fromFile(
        file = File("data/abp-take/ce1.txt"),
        inputEvents = modularInputEvents[1].map { InputEvent("sender_in_${it.name}") },
        outputEvents = modularOutputEvents[1].map { OutputEvent("sender_out_${it.name}") },
        inputNames = modularInputNames[1].map { "sender_$it" },
        outputNames = modularOutputNames[1].map { "sender_$it" }
    ).single()

    // val C: Int = 6
    // val K: Int? = null
    // val P: Int = 5
    // val T: Int? = 8
    // val N: Int? = 14

    val C: Int = 4
    val K: Int? = null
    val P: Int = 5
    val T: Int? = 20
    val N: Int? = 23

    log.info("Inferring the sender...")
    // val automatonSender = inferrer.basic(
    //     scenarioTree = oldTree,
    //     numberOfStates = C,
    //     maxOutgoingTransitions = K,
    //     maxTransitions = T
    // )
    // val automatonSender = inferrer.basicMin(scenarioTree = oldTree)
    // val automatonSender = inferrer.extendedMin(scenarioTree = oldTree, maxGuardSize = P)
    // val automatonSender = inferrer.extended(
    //     scenarioTree = oldTree,
    //     numberOfStates = C,
    //     maxOutgoingTransitions = K,
    //     maxGuardSize = P,
    //     maxTransitions = T,
    //     maxTotalGuardsSize = N
    // )
    // val automatonSender: Automaton? = inferrer.distributedBasic(
    //     numberOfModules = 1,
    //     compoundScenarioTree = positiveCompoundScenarioTree,
    //     modularScenarioTree = MultiArray.create(1) { positiveCompoundScenarioTree.modular[1] },
    //     modularNumberOfStates = MultiArray.create(1) { C },
    //     modularMaxOutgoingTransitions = MultiArray.createNullable(1) { K },
    //     modularMaxTransitions = MultiArray.createNullable(1) { T }
    // )?.let { it.modules[1] }
    // val automatonSender: Automaton? = inferrer.distributedComplete(
    //     numberOfModules = 1,
    //     compoundScenarioTree = positiveCompoundScenarioTree,
    //     modularScenarioTree = MultiArray.create(1) { positiveCompoundScenarioTree.modular[1] },
    //     negativeCompoundScenarioTree = negativeCompoundScenarioTree,
    //     modularNumberOfStates = MultiArray.create(1) { C },
    //     modularMaxOutgoingTransitions = MultiArray.createNullable(1) { K },
    //     modularMaxGuardSize = MultiArray.create(1) { P },
    //     modularMaxTransitions = MultiArray.createNullable(1) { T }
    // )?.let { it.modules[1] }
    val automatonSender: Automaton? = inferrer.distributedCegis(
        numberOfModules = 1,
        compoundScenarioTree = positiveCompoundScenarioTree,
        modularScenarioTree = MultiArray.create(1) { positiveCompoundScenarioTree.modular[1] },
        // negativeCompoundScenarioTree = negativeCompoundScenarioTree,
        modularNumberOfStates = MultiArray.create(1) { C },
        modularMaxOutgoingTransitions = MultiArray.createNullable(1) { K },
        modularMaxGuardSize = MultiArray.create(1) { P },
        modularMaxTransitions = MultiArray.createNullable(1) { T },
        smvDir = File("data/abp-take/smv")
    )?.let { it.modules[1] }

    if (automatonSender == null) {
        log.failure("Automaton not found")
    } else {
        log.info("Inferred automaton:")
        automatonSender.pprint()
        log.info(
            "Inferred automaton has " +
                "${automatonSender.numberOfStates} states, " +
                "${automatonSender.numberOfTransitions} transitions and " +
                "${automatonSender.totalGuardsSize} nodes"
        )

        automatonSender.dump(outDir)

        if (automatonSender.verify(oldTree))
            log.success("Verify: OK")
        else {
            log.failure("Verify: FAILED")
        }
        if (automatonSender.verify(oldTree))
            log.success("Verify2: OK")
        else {
            log.failure("Verify2: FAILED")
        }
        if (automatonSender.verify(negativeScenario1))
            log.success("Verify negative1: OK")
        else {
            log.failure("Verify negative1: FAILED")
        }
        if (automatonSender.verify(negativeScenario.project(1)))
            log.success("Verify negative[1]: OK")
        else {
            log.failure("Verify negative[1]: FAILED")
        }

        println("Mapping:")
        val automatonMapping = automatonSender.map(oldScenario)
        for ((element, state) in oldScenario.elements.zip(automatonMapping)) {
            println("  - $element -> ${state?.id}")
            if (state == null) break
        }
    }

    log.success("All done in %.3f seconds".format(timeSince(timeStart).seconds))
}
