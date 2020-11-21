package ru.ifmo.fbsat.core.task.distributed.complete

import com.github.lipen.multiarray.MultiArray
import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.multiArrayOf
import ru.ifmo.fbsat.core.utils.multiArrayOfNulls
import ru.ifmo.fbsat.core.utils.project
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.withIndex
import java.io.File

@Suppress("DuplicatedCode", "LocalVariableName")
fun Inferrer.distributedCegis2(
    numberOfModules: Int, // M
    compoundScenarioTree: PositiveCompoundScenarioTree, // TEMPORARILY
    modularScenarioTree: MultiArray<PositiveScenarioTree>,
    negativeCompoundScenarioTree: NegativeCompoundScenarioTree? = null,
    // modularNumberOfStates: MultiArray<Int>, // [C]
    modularMaxOutgoingTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [K]
    modularMaxGuardSize: MultiArray<Int>, // [P]
    modularMaxTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [T]
    modularMaxTotalGuardsSize: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [N]
    modularIsEncodeReverseImplication: MultiArray<Boolean> = MultiArray.create(numberOfModules) { true },
    maxTransitions: Int? = null, // T_sum, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N_sum, unconstrained if null
    smvDir: File,
    startD: Int = 1,
): DistributedAutomaton? {
    log.info("Performing distributed CEGIS (second version)...")

    // Copy smv files to output directory
    smvDir.copyRecursively(outDir, overwrite = true)

    // =====
    val M = numberOfModules
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
    // =====

    var negativeTree: NegativeCompoundScenarioTree? = negativeCompoundScenarioTree
    var D = 1

    for (iterationNumber in 1 until 10000) {
        // log.info("CEGIS iteration #$iterationNumber")
        val timeStart = PerformanceCounter.reference

        val automaton = completeMin__(
            numberOfModules = numberOfModules,
            compoundScenarioTree = compoundScenarioTree,
            modularScenarioTree = modularScenarioTree,
            negativeCompoundScenarioTree = negativeTree,
            // modularNumberOfStates = modularNumberOfStates,
            // modularMaxOutgoingTransitions = modularMaxOutgoingTransitions,
            modularMaxGuardSize = modularMaxGuardSize,
            startD = D
        )
        if (automaton == null) {
            log.failure("CEGIS iteration #$iterationNumber failed in %.3f s".format(timeSince(timeStart).seconds))
            return null
        }

        negativeTree = solver.context["negativeCompoundScenarioTree"]
        requireNotNull(negativeTree)
        D = automaton.modular.values.map { it.numberOfStates }.maxOrNull()!!

        // ==============
        for (m in 1..M) {
            // Dump intermediate automaton
            automaton.project(m).dump(outDir, "_${modularName[m]}_iter%04d".format(iterationNumber))
            // Print intermediate automaton
            log.info("Intermediate inferred automaton (module $m):")
            automaton.project(m).pprint()
        }
        // =============

        // Verify automaton with NuSMV
        val counterexamples = automaton.verifyWithNuSMV(
            dir = outDir,
            modularModuleName = multiArrayOf("Sender", "Receiver")
        )
        if (counterexamples.isEmpty()) {
            log.success("CEGIS iteration #$iterationNumber done in %.3f s".format(timeSince(timeStart).seconds))
            log.success("No counterexamples!")
            log.info("Final compound negative scenario tree size: ${negativeTree.size}")
            log.info("Final number of negative scenarios: ${negativeTree.scenarios.size}")
            return automaton
        }

        // Convert counterexamples to negative scenarios
        val negativeScenarios = counterexamples.map {
            NegativeCompoundScenario.fromCounterexample(
                counterexample = it,
                M = M,
                modularName = modularName,
                modularInputEvents = modularInputEvents,
                modularOutputEvents = modularOutputEvents,
                modularInputNames = modularInputNames,
                modularOutputNames = modularOutputNames
            )
        }
        println("Got ${negativeScenarios.size} new negative scenarios of sizes ${negativeScenarios.map { it.elements.size }} with loops ${negativeScenarios.map { it.loopPosition }}")

        // Populate negTree with new negative scenarios
        for (scenario in negativeScenarios) {
            negativeTree.addScenario(scenario)
        }

        // Verify negative scenarios
        for ((i, negScenario) in negativeScenarios.withIndex(start = 1)) {
            if (automaton.verify(negScenario)) {
                log.failure("Verify negative scenario #$i: disproved")

                println("Mapping of negScenario (loopBack = ${negScenario.loopPosition}):")
                for ((j, state) in automaton.map(negScenario).withIndex(start = 1)) {
                    val element = negScenario.elements[j - 1].modular[1]
                    println("[$j / ${negScenario.elements.size}] Compound state: ${state?.values?.map { it.id }} for element = $element (nodeId = ${element.nodeId})" + if (j == negScenario.loopPosition) " [LOOP-BACK]" else "")
                }

                error("sad")
            } else {
                log.success("Verify negative scenario #$i: confirmed")
            }
        }

        log.success("CEGIS iteration #$iterationNumber done in %.3f s".format(timeSince(timeStart).seconds))
    }
    return null
}
