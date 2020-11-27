@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.cli.entry

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.solver.MiniSatSolver
import com.github.lipen.satlib.solver.Solver
import com.soywiz.klock.DateTime
import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.negative.readCounterexamplesFromFile
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.distributed.complete.distributedCegis2
import ru.ifmo.fbsat.core.utils.EpsilonOutputEvents
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.StartStateAlgorithms
import ru.ifmo.fbsat.core.utils.mylog
import ru.ifmo.fbsat.core.utils.multiArrayOf
import ru.ifmo.fbsat.core.utils.project
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.toMultiArray
import java.io.File

fun main() {
    val timeStart = PerformanceCounter.reference
    mylog.br(DateTime.nowLocal().format("yyyy-MM-dd HH:mm:ss"))

    val M = 2
    val modularName = multiArrayOf(
        "sender",
        "receiver"
    )
    val modularInputEvents = MultiArray.create(M) {
        listOf("REQ").map { InputEvent(it) }
    }
    val modularOutputEvents = MultiArray.create(M) {
        listOf("CNF").map { OutputEvent(it) }
    }
    val modularInputNames = multiArrayOf(
        listOf("send", "timeout", "acknowledge", "input_bit"),
        listOf("packet", "input_bit")
    )
    val modularOutputNames = multiArrayOf(
        listOf("done", "packet", "output_bit"),
        listOf("deliver", "acknowledge", "output_bit")
    )
    val solver: Solver = MiniSatSolver()
    val outDir = File("out/abp-take2")
    // val outDir = File("out/abp-100x50-all")
    // val outDir = File("out/abp-10x25-all_new")
    val inferrer = Inferrer(solver, outDir)

    check(modularOutputNames.values.all { it.size == 3 })
    Globals.START_STATE_ALGORITHMS = StartStateAlgorithms.ANY
    Globals.INITIAL_OUTPUT_VALUES = OutputValues.zeros(3)
    Globals.IS_FORBID_TRANSITIONS_TO_FIRST_STATE = false
    Globals.EPSILON_OUTPUT_EVENTS = EpsilonOutputEvents.NONE
    // Globals.IS_BFS_AUTOMATON = false
    // Globals.IS_BFS_GUARD = false
    Globals.IS_DEBUG = true
    Globals.modularName = listOf("sender", "receiver").toMultiArray()

    // val fileTraces = File("data/abp-take/trace.xml")
    // val fileTraces = File("data/abp-take/trace_100_50.xml")
    // val fileTraces = File("data/abp-take/trace_10_25.xml")
    // val fileTraces = File("data/abp-take/trace_10_50.xml")
    val fileTraces = File("data/abp-take/trace_1_100.xml")
    val traces = readCounterexamplesFromFile(fileTraces)
    val scenarios = traces.map { trace ->
        PositiveCompoundScenario.fromCounterexample(
            counterexample = trace,
            M = M,
            modularName = modularName,
            modularInputEvents = modularInputEvents,
            modularOutputEvents = modularOutputEvents,
            modularInputNames = modularInputNames,
            modularOutputNames = modularOutputNames
        )
    }

    // val xml = XML {
    //     unknownChildHandler = { input, isAttribute, name, candidates ->
    //         println("unknown child, whatever this means!")
    //         println("  - input: $input")
    //         println("  - isAttribute: $isAttribute")
    //         println("  - name: $name")
    //         println("  - candidates: $candidates")
    //     }
    // }
    // val sXml = File("data/abp-take/trace.xml").readText().replace("<loops> </loops>", "<loops/>")
    // val trace = xml.parse(THE_Counterexample.serializer(), sXml)
    // println("trace = "); pp(trace)
    // val states: List<Map<String, Boolean>> = trace.nodes.map { node ->
    //     val state = node.states.single()
    //     state.values.associate { value ->
    //         value.variable to value.content.toBoolean()
    //     }
    // }
    // print("states[0] = "); pp(states[0])

    // val traceElementsAll = trace.nodes
    //     .map { node ->
    //         node.states.single().values.associate { value ->
    //             value.variable to value.content.toBoolean()
    //         }
    //     }
    //     .zipWithNext { inputData, outputData ->
    //         CompoundScenarioElement(
    //             modular = MultiArray.create(M) { (m) ->
    //                 ScenarioElement(
    //                     inputAction = InputAction(
    //                         event = modularInputEvents[m].firstOrNull {
    //                             inputData.getValue("${modularName[m]}$${it.name}")
    //                         },
    //                         values = InputValues(modularInputNames[m].map {
    //                             inputData.getValue("${modularName[m]}$$it")
    //                         })
    //                     ),
    //                     outputAction = OutputAction(
    //                         event = modularOutputEvents[m].firstOrNull {
    //                             outputData.getValue("${modularName[m]}.${it.name}")
    //                         },
    //                         values = OutputValues(modularOutputNames[m].map {
    //                             outputData.getValue("${modularName[m]}.$it")
    //                         })
    //                     )
    //                 )
    //             }
    //         )
    //     }
    // val traceElements = traceElementsAll //.take(20)
    // // val traceElements = traceElementsAll.filter { element ->
    // //     element.modularInputEvent.values.any { it != null }
    // // }
    // // for (i in elements.indices) {
    // //     if (elements[i].modularInputAction.values.any { it.event == null }) {
    // //         log.warn("null input event in element i = $i: ${elements[i]}")
    // //     }
    // // }
    // val positiveCompoundScenario = PositiveCompoundScenario(M, traceElements)
    // // println("scenario = $scenario")
    // // println("traceElementsAll (${traceElementsAll.size}):")
    // // for ((i, element) in traceElementsAll.withIndex(start = 1)) {
    // //     println("  - [$i / ${traceElementsAll.size}] $element")
    // // }
    // // println("traceElements (${traceElements.size}):")
    // // for ((i, element) in traceElements.withIndex(start = 1)) {
    // //     println("  - [$i / ${traceElements.size}] $element")
    // // }

    val positiveCompoundScenarioTree = PositiveCompoundScenarioTree(
        M = M,
        modularInputEvents = modularInputEvents,
        modularOutputEvents = modularOutputEvents,
        modularInputNames = modularInputNames,
        modularOutputNames = modularOutputNames
    )
    for (scenario in scenarios) {
        positiveCompoundScenarioTree.addScenario(scenario)
    }
    // positiveCompoundScenarioTree.addScenario(positiveCompoundScenario)
    mylog.info("Positive scenarios: ${scenarios.size}")
    mylog.info("Positive compound scenario tree size: ${positiveCompoundScenarioTree.size}")
    mylog.info("Positive scenario trees sizes: ${positiveCompoundScenarioTree.modular.values.map { it.size }}")

    // ===== Counterexample

    // val counterexamples = readCounterexamplesFromFile(File("data/abp-take/ce.xml"))
    // val negativeCompoundScenarioTree = NegativeCompoundScenarioTree(
    //     M = M,
    //     modularInputEvents = modularInputEvents,
    //     modularOutputEvents = modularOutputEvents,
    //     modularInputNames = modularInputNames,
    //     modularOutputNames = modularOutputNames
    // )
    // for (ce in counterexamples) {
    //     val negativeScenario = NegativeCompoundScenario.fromCounterexample(
    //         counterexample = ce,
    //         M = M,
    //         modularName = modularName,
    //         modularInputEvents = modularInputEvents,
    //         modularOutputEvents = modularOutputEvents,
    //         modularInputNames = modularInputNames,
    //         modularOutputNames = modularOutputNames
    //     )
    //     negativeCompoundScenarioTree.addScenario(negativeScenario)
    //     println("negative scenario (size=${negativeScenario.elements.size}, loop=${negativeScenario.loopPosition}):")
    //     for ((i, element) in negativeScenario.elements.withIndex(start = 1)) {
    //         println("  - [$i / ${negativeScenario.elements.size}] $element")
    //     }
    // }

    // println("negativeCompoundScenarioTree.project(1).uniqueInputs: ${negativeCompoundScenarioTree.project(1).uniqueInputs}")
    //
    // val negativeScenario1 = NegativeScenario.fromFile(
    //     file = File("data/abp-take/ce1.txt"),
    //     inputEvents = modularInputEvents[1].map { InputEvent("sender_in_${it.name}") },
    //     outputEvents = modularOutputEvents[1].map { OutputEvent("sender_out_${it.name}") },
    //     inputNames = modularInputNames[1].map { "sender_$it" },
    //     outputNames = modularOutputNames[1].map { "sender_$it" }
    // ).single()

    // val negativeCompoundScenarioTree = NegativeCompoundScenarioTree(
    //     M = 1,
    //     modularInputEvents = modularInputEvents,
    //     modularOutputEvents = modularOutputEvents,
    //     modularInputNames = modularInputNames,
    //     modularOutputNames = modularOutputNames
    // )
    // val counterexamples = readCounterexamplesFromFile(File("allce.xml"))
    // for (ce in counterexamples) {
    //     negativeCompoundScenarioTree.addScenario(
    //         NegativeCompoundScenario.fromCounterexample(
    //             counterexample = ce,
    //             M = 1,
    //             modularName = modularName,
    //             modularInputEvents = modularInputEvents,
    //             modularOutputEvents = modularOutputEvents,
    //             modularInputNames = modularInputNames,
    //             modularOutputNames = modularOutputNames
    //         )
    //     )
    // }

    // val C: Int = 6
    // val K: Int? = null
    // val P: Int = 5
    // val T: Int? = 8
    // val N: Int? = 14

    val C1: Int = 3
    val K1: Int? = null
    val P1: Int = 4 // 4 min
    val T1: Int? = 7 // 6 min
    val N1: Int? = 13 // 11 min

    val C2: Int = 4
    val K2: Int? = null
    val P2: Int = 4  // 4 min
    val T2: Int? = 7 // 8 min
    val N2: Int? = 7 // 28 min

    mylog.info("Inferring the sender...")
    // val distributedAutomaton = inferrer.distributedBasic(
    //     numberOfModules = M,
    //     compoundScenarioTree = positiveCompoundScenarioTree,
    //     modularScenarioTree = positiveCompoundScenarioTree.modular,
    //     modularNumberOfStates = multiArrayOf(C1, C2),
    //     modularMaxOutgoingTransitions = multiArrayOf(K1, K2),
    //     modularMaxTransitions = multiArrayOf(T1, T2)
    // )
    // val distributedAutomaton = inferrer.distributedBasicMinC(
    //     numberOfModules = M,
    //     compoundScenarioTree = positiveCompoundScenarioTree,
    //     modularScenarioTree = positiveCompoundScenarioTree.modular
    // )
    // val distributedAutomaton = inferrer.distributedExtended(
    //     numberOfModules = M,
    //     compoundScenarioTree = positiveCompoundScenarioTree,
    //     modularScenarioTree = positiveCompoundScenarioTree.modular,
    //     modularNumberOfStates = multiArrayOf(C1, C2),
    //     modularMaxOutgoingTransitions = multiArrayOf(K1, K2),
    //     modularMaxGuardSize = multiArrayOf(P1, P2),
    //     modularMaxTransitions = multiArrayOf(T1, T2),
    //     modularMaxTotalGuardsSize = multiArrayOf(N1, N2)
    // )
    // val distributedAutomaton = inferrer.distributedComplete(
    //     numberOfModules = M,
    //     compoundScenarioTree = positiveCompoundScenarioTree,
    //     modularScenarioTree = positiveCompoundScenarioTree.modular,
    //     negativeCompoundScenarioTree = negativeCompoundScenarioTree,
    //     modularNumberOfStates = multiArrayOf(C1, C2),
    //     modularMaxOutgoingTransitions = multiArrayOf(K1, K2),
    //     modularMaxGuardSize = multiArrayOf(P1, P2),
    //     modularMaxTransitions = multiArrayOf(T1, T2),
    //     modularMaxTotalGuardsSize = multiArrayOf(N1, N2)
    // )
    // val distributedAutomaton = inferrer.distributedCegis(
    //     numberOfModules = M,
    //     compoundScenarioTree = positiveCompoundScenarioTree,
    //     modularScenarioTree = positiveCompoundScenarioTree.modular,
    //     // negativeCompoundScenarioTree = negativeCompoundScenarioTree,
    //     modularNumberOfStates = multiArrayOf(C1, C2),
    //     modularMaxOutgoingTransitions = multiArrayOf(K1, K2),
    //     modularMaxGuardSize = multiArrayOf(P1, P2),
    //     modularMaxTransitions = multiArrayOf(T1, T2),
    //     modularMaxTotalGuardsSize = multiArrayOf(N1, N2),
    //     smvDir = File("data/abp-take/smv")
    // )
    // val distributedAutomaton = inferrer.distributedCegis(
    //     numberOfModules = M,
    //     compoundScenarioTree = positiveCompoundScenarioTree,
    //     modularScenarioTree = positiveCompoundScenarioTree.modular,
    //     // negativeCompoundScenarioTree = negativeCompoundScenarioTree,
    //     modularNumberOfStates = multiArrayOf(C1, C2),
    //     modularMaxOutgoingTransitions = multiArrayOf(K1, K2),
    //     modularMaxGuardSize = multiArrayOf(P1, P2),
    //     modularMaxTransitions = multiArrayOf(T1, T2),
    //     modularMaxTotalGuardsSize = multiArrayOf(N1, N2),
    //     smvDir = File("data/abp-take/smv")
    // )
    val distributedAutomaton = inferrer.distributedCegis2(
        numberOfModules = M,
        compoundScenarioTree = positiveCompoundScenarioTree,
        modularScenarioTree = positiveCompoundScenarioTree.modular,
        // negativeCompoundScenarioTree = negativeCompoundScenarioTree,
        // modularNumberOfStates = multiArrayOf(C1, C2),
        // modularMaxOutgoingTransitions = multiArrayOf(K1, K2),
        modularMaxGuardSize = multiArrayOf(P1, P2),
        // modularMaxTransitions = multiArrayOf(T1, T2),
        // modularMaxTotalGuardsSize = multiArrayOf(N1, N2),
        smvDir = File("data/abp-take/smv")
    )
    // val distributedAutomaton = inferrer.distributedCegis2(
    //     numberOfModules = M,
    //     compoundScenarioTree = positiveCompoundScenarioTree,
    //     modularScenarioTree = positiveCompoundScenarioTree.modular,
    //     // negativeCompoundScenarioTree = negativeCompoundScenarioTree,
    //     // modularNumberOfStates = multiArrayOf(C1, C2),
    //     // modularMaxOutgoingTransitions = multiArrayOf(K1, K2),
    //     modularMaxGuardSize = multiArrayOf(P1, P2),
    //     // modularMaxTransitions = multiArrayOf(T1, T2),
    //     // modularMaxTotalGuardsSize = multiArrayOf(N1, N2),
    //     smvDir = File("data/abp-take/smv"),
    //     startD = 4
    // )

    if (distributedAutomaton == null) {
        mylog.failure("Inference failed")
    } else {
        mylog.success("Inference succeeded!")

        for (m in 1..M) {
            val automaton = distributedAutomaton.project(m)
            val name = modularName[m]
            mylog.info("Inferred $name:")
            automaton.pprint()
            mylog.info(
                "Inferred $name has " +
                    "${automaton.numberOfStates} states, " +
                    "${automaton.numberOfTransitions} transitions and " +
                    "${automaton.totalGuardsSize} nodes"
            )

            automaton.dump(outDir, name = name)

            if (automaton.verify(positiveCompoundScenarioTree.project(m))) {
                mylog.success("Verify: OK")
            } else {
                mylog.failure("Verify: FAILED")
            }
            mylog.br()
        }
    }

    // val distributedAutomaton: DistributedAutomaton? = inferrer.distributedCegis(
    //     numberOfModules = 1,
    //     compoundScenarioTree = positiveCompoundScenarioTree,
    //     modularScenarioTree = MultiArray.create(1) { positiveCompoundScenarioTree.project(1) },
    //     // negativeCompoundScenarioTree = negativeCompoundScenarioTree,
    //     modularNumberOfStates = MultiArray.create(1) { C },
    //     modularMaxOutgoingTransitions = MultiArray.createNullable(1) { K },
    //     modularMaxGuardSize = MultiArray.create(1) { P },
    //     modularMaxTransitions = MultiArray.createNullable(1) { T },
    //     smvDir = File("data/abp-take/smv")
    // )

    // if (distributedAutomaton == null ) {
    //     log.failure("Distributed automaton not found")
    // } else {
    //     log.success("Inferred distributed automaton has: " +
    //         "C = ${distributedAutomaton.modular.map { it.numberOfStates }.values.joinToString("+") { it.toString() }} = ${distributedAutomaton.numberOfStates}, " +
    //         "T = ${distributedAutomaton.modular.map { it.numberOfTransitions }.values.joinToString("+") { it.toString() }} = ${distributedAutomaton.numberOfTransitions}, " +
    //         "N = ${distributedAutomaton.modular.map { it.totalGuardsSize }.values.joinToString("+") { it.toString() }} = ${distributedAutomaton.totalGuardsSize}")
    // }

    mylog.br(DateTime.nowLocal().format("yyyy-MM-dd HH:mm:ss"))
    mylog.success("All done in %.3f seconds".format(timeSince(timeStart).seconds))
}
