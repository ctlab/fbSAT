package ru.ifmo.fbsat.core.task.single.complete

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.Counterexample
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.single.basic.BasicTask
import ru.ifmo.fbsat.core.task.single.extended.ExtendedTask
import ru.ifmo.fbsat.core.task.single.extended.extendedMin
import ru.ifmo.fbsat.core.task.single.extended.extendedMinUB
import ru.ifmo.fbsat.core.task.single.extended.inferExtended
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.timeSince
import java.io.File

private val logger = MyLogger {}

fun Inferrer.cegis(
    scenarioTree: PositiveScenarioTree,
    negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    smvDir: File,
    loopNumber: Int = 0,
): Automaton? {
    // val timeStart = PerformanceCounter.reference
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
    val automaton = performCegis(smvDir, loopNumber)
    // logger.info("Task cegis done in %.2f s".format(timeSince(timeStart).seconds))
    // if (solver.isSupportStats()) {
    //     logger.debug("Propagations: ${solver.numberOfPropagations()}")
    //     logger.debug("Conflicts: ${solver.numberOfConflicts()}")
    //     logger.debug("Decisions: ${solver.numberOfDecisions()}")
    // }
    if (Globals.IS_DUMP_CNF) {
        solver.dumpDimacs(outDir.resolve("cnf_cegis_C${numberOfStates}_K${maxOutgoingTransitions}_P${maxGuardSize}_T${maxTransitions}_N${maxTotalGuardsSize}_loop${loopNumber}.cnf"))
    }
    return automaton
}

@Suppress("LocalVariableName")
fun Inferrer.cegisMin(
    scenarioTree: PositiveScenarioTree,
    initialNegativeScenarioTree: NegativeScenarioTree? = null,
    numberOfStates: Int? = null,
    startNumberOfStates: Int? = null,
    maxGuardSize: Int? = null, // P, search if null
    maxPlateauWidth: Int? = null, // w, =Inf if null
    smvDir: File,
): Automaton? {
    val extendedAutomaton = if (maxGuardSize == null) {
        extendedMinUB(scenarioTree, numberOfStates = startNumberOfStates, maxPlateauWidth = maxPlateauWidth)
    } else {
        extendedMin(scenarioTree, numberOfStates = numberOfStates, maxGuardSize = maxGuardSize)
    }
    checkNotNull(extendedAutomaton)
    val C = extendedAutomaton.numberOfStates
    // Note: reusing K from extMinTask may fail!
    val K = if (Globals.IS_REUSE_K) extendedAutomaton.maxOutgoingTransitions else C
    logger.info("Using K = $K")
    val P = extendedAutomaton.maxGuardSize
    var N = extendedAutomaton.totalGuardsSize

    logger.info("extendedAutomaton:")
    extendedAutomaton.pprint()
    logger.info("extendedAutomaton has C = $C, P = $P, N = $N")

    val negativeScenarioTree = initialNegativeScenarioTree ?: NegativeScenarioTree(scenarioTree)

    for (loopNumber in 1..100) {
        logger.just("===== Loop number #$loopNumber, N = $N =====")

        val automaton = cegis(
            scenarioTree = scenarioTree,
            negativeScenarioTree = negativeScenarioTree,
            numberOfStates = C,
            maxOutgoingTransitions = K,
            maxGuardSize = P,
            maxTotalGuardsSize = N,
            smvDir = smvDir,
            loopNumber = loopNumber
        )
        if (automaton != null) {
            logger.just("Hooray! Minimal full verified automaton has been found!")
            return automaton
        } else {
            logger.just("UNSAT, N = $N is too small, trying larger value...")
            N += 1
            if (N > C * K * P) {
                logger.error("N reached upper bound C*K*P = ${C * K * P}")
                break
            }
        }
    }
    return null
}

@Suppress("DuplicatedCode")
fun Inferrer.performCegis(smvDir: File, loopNumber: Int): Automaton? {
    logger.info("Performing CEGIS...")

    // Copy smv files to output directory
    smvDir.copyRecursively(outDir, overwrite = true)

    val scenarioTree: PositiveScenarioTree = solver.context["scenarioTree"]
    val negativeScenarioTree: NegativeScenarioTree = solver.context["negativeScenarioTree"]
    lateinit var lastNegativeScenarios: List<NegativeScenario>

    for (iterationNumber in 1 until 10000) {
        logger.debug("CEGIS iteration #$iterationNumber on loop $loopNumber...")
        val timeStart = PerformanceCounter.reference

        // Update to take into account possible extension of the negative scenario tree
        solver.updateNegativeReduction()
        // Infer update
        val automaton = inferExtended()
        if (Globals.IS_DUMP_CNF) {
            solver.dumpDimacs(outDir.resolve("cnf_cegis_loop${loopNumber}_iter${iterationNumber}.cnf"))
        }
        if (automaton == null) {
            logger.error(
                "CEGIS iteration #$iterationNumber on loop $loopNumber failed to infer an automaton after %.3f s"
                    .format(timeSince(timeStart).seconds)
            )
            return null
        }
        // ==============
        // Dump intermediate automaton
        automaton.dump(outDir, "_automaton_loop%d_iter%04d".format(loopNumber, iterationNumber))
        // ==============
        // Verify automaton with NuSMV
        val counterexamples = automaton.verifyWithNuSMV(outDir)
        if (counterexamples.isEmpty()) {
            logger.info("CEGIS iteration #$iterationNumber done in %.3f s".format(timeSince(timeStart).seconds))
            logger.info("No counterexamples!")
            return automaton
        }
        // Convert counterexamples to negative scenarios
        val negativeScenarios = counterexamples.map {
            NegativeScenario.from(
                counterexample = it,
                inputEvents = scenarioTree.inputEvents,
                outputEvents = scenarioTree.outputEvents,
                inputNames = scenarioTree.inputNames,
                outputNames = scenarioTree.outputNames
            )
        }
        // Populate negTree with new negative scenarios
        val treeSize = negativeScenarioTree.size
        for (scenario in negativeScenarios) {
            negativeScenarioTree.addScenario(scenario)
        }
        val treeSizeDiff = negativeScenarioTree.size - treeSize
        // Note: it is suffice to check just `negSc == lastNegSc`, but it may be costly,
        // so check it only in a specific case - when negative tree does not change its size
        if (treeSizeDiff == 0 && negativeScenarios == lastNegativeScenarios) {
            error("Stale")
        }
        lastNegativeScenarios = negativeScenarios
        logger.info("CEGIS iteration #$iterationNumber done in %.3f s".format(timeSince(timeStart).seconds))
    }
    return null
}

fun Automaton.verifyWithNuSMV(dir: File): List<Counterexample> {
    // Save automaton to smv directory
    dumpSmv(dir.resolve("control.smv"))

    // Perform formal verification using NuSMV, generate counterexamples to given ltl-spec
    val cmd = "make model counterexamples"
    logger.debug { "Running '$cmd'..." }
    val timeStart = PerformanceCounter.reference
    val exitcode = Runtime.getRuntime().exec(cmd, null, dir).waitFor()
    val runningTime = timeSince(timeStart)
    logger.debug { "'$cmd' returned with $exitcode in %.3f s.".format(runningTime.seconds) }
    check(exitcode == 0) { "NuSMV exitcode: $exitcode" }

    // Handle counterexamples after verification
    val fileCounterexamples = dir.resolve("counterexamples")
    return if (fileCounterexamples.exists()) {
        // Read new counterexamples
        val counterexamples: List<Counterexample> = Counterexample.from(fileCounterexamples)

        // [DEBUG] Append new counterexamples to 'ce'
        // logger.debug { "Dumping ${counterexamples.size} counterexample(s)..." }
        // dir.resolve("ce").appendText(fileCounterexamples.readText())

        counterexamples
    } else {
        emptyList()
    }
}
