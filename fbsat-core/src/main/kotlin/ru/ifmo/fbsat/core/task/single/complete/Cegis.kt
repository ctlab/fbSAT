package ru.ifmo.fbsat.core.task.single.complete

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.Counterexample
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.completeVars
import ru.ifmo.fbsat.core.task.single.basic.BasicTask
import ru.ifmo.fbsat.core.task.single.extended.ExtendedTask
import ru.ifmo.fbsat.core.task.single.extended.extendedMin
import ru.ifmo.fbsat.core.task.single.extended.extendedMinUB
import ru.ifmo.fbsat.core.task.single.extended.inferExtended
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince
import java.io.File

fun Inferrer.cegis(
    scenarioTree: ScenarioTree,
    negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    smvDir: File
): Automaton? {
    reset()
    declare(
        BasicTask(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = maxTransitions,
            isEncodeReverseImplication = false
        )
    )
    declare(ExtendedTask(maxGuardSize = maxGuardSize, maxTotalGuardsSize = maxTotalGuardsSize))
    declare(CompleteTask(negativeScenarioTree))
    return performCegis(smvDir)
}

@Suppress("LocalVariableName")
fun Inferrer.cegisMin(
    scenarioTree: ScenarioTree,
    initialNegativeScenarioTree: NegativeScenarioTree? = null,
    numberOfStates: Int? = null,
    maxGuardSize: Int? = null, // P, search if null
    maxPlateauWidth: Int? = null, // w, =Inf if null
    smvDir: File
): Automaton? {
    val extendedAutomaton = if (maxGuardSize == null) {
        extendedMinUB(scenarioTree, numberOfStates = numberOfStates, maxPlateauWidth = maxPlateauWidth)
    } else {
        extendedMin(scenarioTree, numberOfStates = numberOfStates, maxGuardSize = maxGuardSize)
    }
    checkNotNull(extendedAutomaton)
    val C = extendedAutomaton.numberOfStates
    // Note: reusing K from extMinTask may fail!
    val K = if (Globals.IS_REUSE_K) extendedAutomaton.maxOutgoingTransitions else C
    log.info("Using K = $K")
    val P = extendedAutomaton.maxGuardSize
    var N = extendedAutomaton.totalGuardsSize

    log.info("extendedAutomaton:")
    extendedAutomaton.pprint()
    log.info("extendedAutomaton has C = $C, P = $P, N = $N")

    val negativeScenarioTree = initialNegativeScenarioTree ?: NegativeScenarioTree(
        inputEvents = scenarioTree.inputEvents,
        outputEvents = scenarioTree.outputEvents,
        inputNames = scenarioTree.inputNames,
        outputNames = scenarioTree.outputNames
    )

    for (loopNumber in 1..100) {
        log.just("===== Loop number #$loopNumber, N = $N =====")

        val automaton = cegis(
            scenarioTree = scenarioTree,
            negativeScenarioTree = negativeScenarioTree,
            numberOfStates = C,
            maxOutgoingTransitions = K,
            maxGuardSize = P,
            maxTotalGuardsSize = N,
            smvDir = smvDir
        )
        if (automaton != null) {
            log.just("Hooray! Minimal full verified automaton has been found!")
            return automaton
        } else {
            log.just("UNSAT, N = $N is too small, trying larger value...")
            N += 1
            if (N > C * K * P) {
                log.error("N reached upper bound C*K*P = ${C * K * P}")
                break
            }
        }
    }
    return null
}

fun Inferrer.performCegis(smvDir: File): Automaton? {
    log.info("Performing CEGIS...")

    // Copy smv files to output directory
    smvDir.copyRecursively(outDir, overwrite = true)

    val vars = solver.context.completeVars
    val scenarioTree = vars.scenarioTree
    val negativeScenarioTree = vars.negativeScenarioTree
    lateinit var lastNegativeScenarios: List<NegativeScenario>

    for (iterationNumber in 1 until 10000) {
        // log.info("CEGIS iteration #$iterationNumber")
        val timeStart = PerformanceCounter.reference

        // Update to take into account possible extension of the negative scenario tree
        solver.updateNegativeReduction()
        // Infer update
        val automaton = inferExtended()
        if (automaton == null) {
            log.failure("CEGIS iteration #$iterationNumber done in %.3f s".format(timeSince(timeStart).seconds))
            return null
        }
        // ==============
        // Dump intermediate automaton
        automaton.dump(outDir, "_automaton_iter%04d".format(iterationNumber))
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
            NegativeScenario.fromCounterexample(
                it,
                scenarioTree.inputEvents,
                scenarioTree.outputEvents,
                scenarioTree.inputNames,
                scenarioTree.outputNames
            )
        }
        // Populate negTree with new negative scenarios
        val treeSize = negativeScenarioTree.size
        negativeScenarios.forEach(negativeScenarioTree::addNegativeScenario)
        val treeSizeDiff = negativeScenarioTree.size - treeSize
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

fun Automaton.verifyWithNuSMV(dir: File): List<Counterexample> {
    // Save automaton to smv directory
    dumpSmv(dir.resolve("control.smv"))

    // Perform formal verification using NuSMV, generate counterexamples to given ltl-spec
    val cmd = "make clean model counterexamples"
    log.debug { "Running '$cmd'..." }
    val timeStart = PerformanceCounter.reference
    val exitcode = Runtime.getRuntime().exec(cmd, null, dir).waitFor()
    val runningTime = timeSince(timeStart)
    log.debug { "'$cmd' returned with $exitcode in %.3f s.".format(runningTime.seconds) }
    check(exitcode == 0) { "NuSMV exitcode: $exitcode" }

    // Handle counterexamples after verification
    val fileCounterexamples = dir.resolve("counterexamples")
    return if (fileCounterexamples.exists()) {
        // Read new counterexamples
        val counterexamples: List<Counterexample> = Counterexample.fromFile(fileCounterexamples)

        // [DEBUG] Append new counterexamples to 'ce'
        log.debug { "Dumping ${counterexamples.size} counterexample(s)..." }
        dir.resolve("ce").appendText(fileCounterexamples.readText())

        counterexamples
    } else {
        emptyList()
    }
}
