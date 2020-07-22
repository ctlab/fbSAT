package ru.ifmo.fbsat.core.task.distributed.complete

import com.github.lipen.multiarray.MultiArray
import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.negative.THE_Counterexample
import ru.ifmo.fbsat.core.scenario.negative.readCounterexampleFromFile
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.distributed.basic.DistributedBasicTask
import ru.ifmo.fbsat.core.task.distributed.extended.DistributedExtendedTask
import ru.ifmo.fbsat.core.task.distributed.extended.inferDistributedExtended
import ru.ifmo.fbsat.core.task.distributedCompleteVars
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.multiArrayOfNulls
import ru.ifmo.fbsat.core.utils.timeSince
import java.io.File

fun Inferrer.distributedCegis(
    numberOfModules: Int, // M
    compoundScenarioTree: PositiveCompoundScenarioTree, // TEMPORARILY
    modularScenarioTree: MultiArray<PositiveScenarioTree>,
    negativeCompoundScenarioTree: NegativeCompoundScenarioTree? = null,
    modularNumberOfStates: MultiArray<Int>, // [C]
    modularMaxOutgoingTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [K]
    modularMaxGuardSize: MultiArray<Int>, // [P]
    modularMaxTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [T]
    modularMaxTotalGuardsSize: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [N]
    modularIsEncodeReverseImplication: MultiArray<Boolean> = MultiArray.create(numberOfModules) { true },
    maxTransitions: Int? = null, // T_sum, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N_sum, unconstrained if null
    smvDir: File
): DistributedAutomaton? {
    reset()
    declare(
        DistributedBasicTask(
            numberOfModules = numberOfModules,
            compoundScenarioTree = compoundScenarioTree,
            modularScenarioTree = modularScenarioTree,
            modularNumberOfStates = modularNumberOfStates,
            modularMaxOutgoingTransitions = modularMaxOutgoingTransitions,
            modularMaxTransitions = modularMaxTransitions,
            modularIsEncodeReverseImplication = modularIsEncodeReverseImplication,
            maxTransitions = maxTransitions
        )
    )
    declare(
        DistributedExtendedTask(
            numberOfModules = numberOfModules,
            modularMaxGuardSize = modularMaxGuardSize,
            modularMaxTotalGuardsSize = modularMaxTotalGuardsSize,
            maxTotalGuardsSize = maxTotalGuardsSize
        )
    )
    declare(
        DistributedCompleteTask(
            numberOfModules = numberOfModules,
            negativeCompoundScenarioTree = negativeCompoundScenarioTree
        )
    )
    return performDistributedCegis(smvDir)
}

@Suppress("DuplicatedCode")
fun Inferrer.performDistributedCegis(smvDir: File): DistributedAutomaton? {
    log.info("Performing distributed CEGIS...")

    // Copy smv files to output directory
    smvDir.copyRecursively(outDir, overwrite = true)

    val vars = solver.context.distributedCompleteVars
    val modularPositiveTree = vars.modularScenarioTree
    val negativeTree = vars.negativeCompoundScenarioTree
    lateinit var lastNegativeScenarios: List<NegativeCompoundScenario>

    // =====
    val M = vars.M
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
    // =====

    for (iterationNumber in 1 until 10000) {
        // log.info("CEGIS iteration #$iterationNumber")
        val timeStart = PerformanceCounter.reference

        // Update to take into account possible extension of the negative scenario tree
        solver.updateDistributedNegativeReduction(vars)
        // Infer update
        val automaton = inferDistributedExtended()
        if (automaton == null) {
            log.failure("CEGIS iteration #$iterationNumber done in %.3f s".format(timeSince(timeStart).seconds))
            return null
        }
        // ==============
        // Dump intermediate automaton
        // automaton.dump(outDir, "_automaton_iter%04d".format(iterationNumber))
        // ==============
        // Verify automaton with NuSMV
        val counterexamples = automaton.verifyWithNuSMV(outDir)
        if (counterexamples.isEmpty()) {
            log.success("CEGIS iteration #$iterationNumber done in %.3f s".format(timeSince(timeStart).seconds))
            log.success("These is no counterexamples, nice!")
            return automaton
        }
        // Convert counterexamples to negative scenarios
        val negativeScenarios = counterexamples.map {
            NegativeCompoundScenario.fromCounterexample(
                counterexample = it,
                M = M,
                modularName = modularName,
                inputMapping = inputMapping,
                modularInputEvents = modularInputEvents,
                modularOutputEvents = modularOutputEvents,
                modularInputNames = modularInputNames,
                modularOutputNames = modularOutputNames
            )
        }
        println("Got ${negativeScenarios.size} new negative scenarios of sizes ${negativeScenarios.map { it.elements.size }} with loops ${negativeScenarios.map { it.loopPosition }}")
        // Populate negTree with new negative scenarios
        val treeSize = negativeTree.size
        for (scenario in negativeScenarios) {
            negativeTree.addScenario(scenario)
        }
        val treeSizeDiff = negativeTree.size - treeSize
        // Note: it is suffice to check just `negSc == lastNegSc`, but it may be costly,
        // so check it only in a specific case - when negative tree does not change its size
        if (treeSizeDiff == 0 && negativeScenarios == lastNegativeScenarios) {
            error("Stale")
        }
        lastNegativeScenarios = negativeScenarios
        log.success("CEGIS iteration #$iterationNumber done in %.3f s".format(timeSince(timeStart).seconds))
    }
    return null
}

fun DistributedAutomaton.verifyWithNuSMV(dir: File): List<THE_Counterexample> {
    // Save automaton to smv directory
    dumpSmv(dir)

    // Perform formal verification using NuSMV, generate counterexamples to given ltl-spec
    val cmd = "make clean model counterexamples"
    log.debug { "Running '$cmd'..." }
    val timeStart = PerformanceCounter.reference
    val process = Runtime.getRuntime().exec(cmd, null, dir)
    // process.inputStream.source().useLines { lines ->
    //     for (line in lines) {
    //         println("[stdout] NuSMV: $line")
    //     }
    // }
    // process.errorStream.source().useLines { lines ->
    //     for (line in lines) {
    //         println("[stderr] NuSMV: $line")
    //     }
    // }
    val exitcode = process.waitFor()
    val runningTime = timeSince(timeStart)
    log.debug { "'$cmd' returned with $exitcode in %.3f s.".format(runningTime.seconds) }
    check(exitcode == 0) { "NuSMV exitcode: $exitcode" }

    // Handle counterexamples after verification
    val fileCounterexamples = dir.resolve("counterexamples")
    return if (fileCounterexamples.exists()) {
        // Read new counterexamples
        val counterexamples: List<THE_Counterexample> = readCounterexampleFromFile(fileCounterexamples)

        // [DEBUG] Append new counterexamples to 'ce'
        log.debug { "Dumping ${counterexamples.size} counterexample(s)..." }
        dir.resolve("ce").appendText(fileCounterexamples.readText())

        counterexamples
    } else {
        emptyList()
    }
}
