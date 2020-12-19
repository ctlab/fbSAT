package ru.ifmo.fbsat.core.task.distributed.complete

import com.github.lipen.multiarray.MultiArray
import com.soywiz.klock.PerformanceCounter
import okio.source
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.negative.THE_Counterexample
import ru.ifmo.fbsat.core.scenario.negative.readCounterexamplesFromFile
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.distributed.basic.DistributedBasicTask
import ru.ifmo.fbsat.core.task.distributed.extended.DistributedExtendedTask
import ru.ifmo.fbsat.core.utils.multiArrayOf
import ru.ifmo.fbsat.core.utils.multiArrayOfNulls
import ru.ifmo.fbsat.core.utils.mylog
import ru.ifmo.fbsat.core.utils.project
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.useLines
import ru.ifmo.fbsat.core.utils.withIndex
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
    modularIsEncodeReverseImplication: MultiArray<Boolean> = MultiArray.new(numberOfModules) { true },
    maxTransitions: Int? = null, // T_sum, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N_sum, unconstrained if null
    smvDir: File,
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
    mylog.info("Performing distributed CEGIS...")

    // Copy smv files to output directory
    smvDir.copyRecursively(outDir, overwrite = true)

    val modularScenarioTree: MultiArray<PositiveScenarioTree> = solver.context["modularScenarioTree"]
    val negativeCompoundScenarioTree: NegativeCompoundScenarioTree = solver.context["negativeCompoundScenarioTree"]
    val negativeTree = negativeCompoundScenarioTree
    lateinit var lastNegativeScenarios: List<NegativeCompoundScenario>
    var lastHashCode: Int = -1

    // =====
    val M: Int = solver.context["M"]
    val modularName = multiArrayOf(
        "sender",
        "receiver"
    )
    val modularInputEvents = MultiArray.new(M) {
        listOf("REQ").map { InputEvent(it) }
    }
    val modularOutputEvents = MultiArray.new(M) {
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

    for (iterationNumber in 1 until 10000) {
        // log.info("CEGIS iteration #$iterationNumber")
        val timeStart = PerformanceCounter.reference

        // Update to take into account possible extension of the negative scenario tree
        solver.updateDistributedNegativeReduction()
        // Infer update
        val automaton = inferDistributedComplete()
        if (automaton == null) {
            mylog.failure("CEGIS iteration #$iterationNumber done in %.3f s".format(timeSince(timeStart).seconds))
            return null
        }
        check(automaton.modules.shape.single() == M) { "modules.size must be M = $M" }
        // check(automaton.modules.shape.single() == 1) { "modules.size must be 1" }
        // ==============
        for (m in 1..M) {
            // Dump intermediate automaton
            automaton.project(m).dump(outDir, "_${modularName[m]}_iter%04d".format(iterationNumber))
            // Print intermediate automaton
            mylog.info("Intermediate inferred automaton (module $m):")
            automaton.project(m).pprint()
        }
        // Check that inferred automaton does not satisfy negative scenario tree
        // log.info("Post-infer negative tree verify...")
        // var ok = true
        // for ((i, negScenario) in negativeTree.scenarios.withIndex(start = 1)) {
        //     if (automaton.verify(negScenario)) {
        //         log.success("[$i / ${negativeTree.scenarios.size}] Verify: OK")
        //     } else {
        //         log.failure("[$i / ${negativeTree.scenarios.size}] Verify: FAILED")
        //         ok = false
        //         val model: model = solver.context["lastmodel"]
        //         val C = vars.modularC[1]
        //         val E = vars.modularE[1]
        //         val U = vars.modularCompleteVariables[1].negU
        //         // val assignment =
        //         //     DistributedExtendedAssignment.frommodel(model, solver.context.distributedExtendedVars)
        //         val completeVars = vars.modularCompleteVariables[1]
        //         val negActualTransitionFunction = completeVars.negActualTransitionFunction.convert(model)
        //         for (u in 1..U)
        //             for (c in 1..C)
        //                 for (e in 1..E) {
        //                     println("negActualTransitionFunction[c = $c, e = $e, u = $u = ${negativeTree.modular[1].uniqueInputs[u - 1].values.toBinaryString()}] = ${negActualTransitionFunction[c, e, u]}")
        //                 }
        //         val stateOutputEvent = completeVars.stateOutputEvent.convert(model)
        //         val negMapping = completeVars.negMapping.convert(model)
        //         val negFirstFired = completeVars.negFirstFired.convert(model)
        //         val negTransitionFiring = completeVars.negTransitionFiring.convert(model)
        //         val negTransitionTruthTable = completeVars.negTransitionTruthTable.convert(model)
        //         val actualTransitionFunction = completeVars.actualTransitionFunction.convert(model)
        //         val u = negativeTree.modular[1].uniqueInputs
        //             .indexOf(InputValues("0111".toBooleanList())) + 1
        //         println("===")
        //         println(
        //             "negMapping[tp(v) = ${negativeTree.modular[1].parent(867)}] = " +
        //                 "${negMapping[negativeTree.modular[1].parent(867)]}"
        //         )
        //         println("negMapping[v = 867] = ${negMapping[867]}")
        //         println("negActualTransitionFunction[c=4, e=1, u=$u=0111] = ${negActualTransitionFunction[4, 1, u]}")
        //         println("stateOutputEvent[c=4] = ${stateOutputEvent[4]}")
        //         println("negFirstFired[c=4, e=1, u=$u=0111] = ${negFirstFired[4, 1, u]}")
        //         println("negTransitionFiring[c=4, k=1, e=1, u=$u=0111] = ${negTransitionFiring[4, 1, 1, u]}")
        //         println("negTransitionTruthTable[c=4, k=1, u=$u=0111] = ${negTransitionTruthTable[4, 1, u]}")
        //         println("actualTransitionFunction[c=4, e=1, u=$u=0111] = ${actualTransitionFunction[4, 1, u]}")
        //         // for (p in 1..completeVars.P) {
        //         //     println("nodeType[]")
        //         // }
        //         println("===")
        //     }
        // }
        // check(ok) { "Post-infer negative scenario tree verification failed" }
        // log.success("Post-infer negative tree verify: OK")
        // Check stale via hash code
        val hash = automaton.modules[1].calculateHashCode()
        if (hash == lastHashCode) {
            mylog.warn("Stale (by hash)")
        }
        lastHashCode = hash
        // =============
        // Verify automaton with NuSMV
        val counterexamples = automaton.verifyWithNuSMV(
            dir = outDir,
            modularModuleName = multiArrayOf("Sender", "Receiver")
        )
        if (counterexamples.isEmpty()) {
            mylog.success("CEGIS iteration #$iterationNumber done in %.3f s".format(timeSince(timeStart).seconds))
            mylog.success("No counterexamples!")
            mylog.info("Final compound negative scenario tree size: ${negativeTree.size}")
            mylog.info("Final number of negative scenarios: ${negativeTree.scenarios.size}")
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
        val treeSize = negativeTree.size
        for (scenario in negativeScenarios) {
            negativeTree.addScenario(scenario)
        }
        val treeSizeDiff = negativeTree.size - treeSize
        // Verify negative scenarios
        for ((i, negScenario) in negativeScenarios.withIndex(start = 1)) {
            if (automaton.verify(negScenario)) {
                mylog.failure("Verify negative scenario #$i: disproved")

                println("Mapping of negScenario (loopBack = ${negScenario.loopPosition}):")
                for ((j, state) in automaton.map(negScenario).withIndex(start = 1)) {
                    val element = negScenario.elements[j - 1].project(1)
                    println("[$j / ${negScenario.elements.size}] Compound state: ${state?.values?.map { it.id }} for element = $element (nodeId = ${element.nodeId})" + if (j == negScenario.loopPosition) " [LOOP-BACK]" else "")
                }

                error("sad")
            } else {
                mylog.success("Verify negative scenario #$i: confirmed")
            }
        }
        // Note: it is suffice to check just `negSc == lastNegSc`, but it may be costly,
        // so check it only in a specific case - when negative tree does not change its size
        if (treeSizeDiff == 0 && negativeScenarios == lastNegativeScenarios) {
            error("Stale")
        }
        lastNegativeScenarios = negativeScenarios
        mylog.success("CEGIS iteration #$iterationNumber done in %.3f s".format(timeSince(timeStart).seconds))
    }
    return null
}

fun DistributedAutomaton.verifyWithNuSMV(dir: File, modularModuleName: MultiArray<String>): List<THE_Counterexample> {
    // Save automaton to smv directory
    dumpSmv(dir, modularModuleName)

    // Perform formal verification using NuSMV, generate counterexamples to given ltl-spec
    val cmd = "make model counterexamples"
    mylog.debug { "Running '$cmd'..." }
    val timeStart = PerformanceCounter.reference
    val process = Runtime.getRuntime().exec(cmd, null, dir)
    val exitcode = process.waitFor()
    val runningTime = timeSince(timeStart)
    mylog.debug { "'$cmd' returned with $exitcode in %.3f s.".format(runningTime.seconds) }
    check(exitcode == 0) {
        process.inputStream.source().useLines { lines ->
            for (line in lines) {
                println("[stdout] NuSMV: $line")
            }
        }
        process.errorStream.source().useLines { lines ->
            for (line in lines) {
                println("[stderr] NuSMV: $line")
            }
        }
        "NuSMV exitcode: $exitcode"
    }

    // Handle counterexamples after verification
    val fileCounterexamples = dir.resolve("counterexamples.xml")
    return if (fileCounterexamples.exists()) {
        // Read new counterexamples
        val counterexamples: List<THE_Counterexample> = readCounterexamplesFromFile(fileCounterexamples)

        // [DEBUG] Append new counterexamples to 'ce'
        // log.debug { "Dumping ${counterexamples.size} counterexample(s)..." }
        // dir.resolve("ce").appendText(fileCounterexamples.readText())

        counterexamples
    } else {
        emptyList()
    }
}
