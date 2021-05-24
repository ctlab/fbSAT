package ru.ifmo.fbsat.core.task.single.complete

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.constraints.IncrementalCardinality
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
import ru.ifmo.fbsat.core.utils.*
import java.io.File
import kotlin.math.max

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
    return performCegis(smvDir, loopNumber)
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

    val negativeScenarioTree = initialNegativeScenarioTree ?: NegativeScenarioTree(scenarioTree, true)

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
    val heat = solver.context("heat") { mutableMapOf<Int, MutableSet<Int>>() }

    mainLoop@ for (iterationNumber in 1 until 10000) {
        // log.info("CEGIS iteration #$iterationNumber")
        logger.debug("CEGIS iteration #$iterationNumber on loop $loopNumber")
        val timeStart = PerformanceCounter.reference

        // Update to take into account possible extension of the negative scenario tree
        solver.updateNegativeReduction()
        // Infer update
        val automaton = inferExtended()
        if (automaton == null) {
            logger.error(
                "CEGIS iteration #$iterationNumber failed to infer an automaton after %.3f s"
                    .format(timeSince(timeStart).seconds)
            )
            return null
        }

        // ==============
        // Dump intermediate automaton
        automaton.dump(outDir, "_automaton_loop%d_iter%04d".format(loopNumber, iterationNumber))

        if (solver.context.getOrNull<CutTreeMarker>("isCutTree") != null || solver.context.getOrNull<Cegisic>("cegisic") != null) {
            val set = mutableSetOf<Int>()
            for (scenario in negativeScenarioTree.scenarios) {
                val path = automaton.eval(scenario).toList()
                for ((scenarioElement, pathElement) in scenario.elements zip path) {
                    if (scenarioElement.outputAction != pathElement.outputAction) {
                        break
                    }
                    set += scenarioElement.nodeId!!
                }
            }
            logger.info("Current good vertices = ${set.size + 1}, negative_scenario_tree.size = ${negativeScenarioTree.size}, " +
                "native_negative_tree.size = ${solver.context.get<NegativeScenarioTree>("nativeNegativeTree").size}")
        }
        if (solver.context.getOrNull<CutTreeMarker>("heightTree") != null) {
            var height = 0
            for (scenario in negativeScenarioTree.scenarios) {
                val path = automaton.eval(scenario).toList()
                for ((index, elements) in (scenario.elements zip path).withIndex()) {
                    val (scenarioElement, pathElement) = elements
                    if (scenarioElement.outputAction != pathElement.outputAction) {
                        break
                    }
                    height = max(height, index + 1)
                }
            }
            logger.info("Current height = ${height}, negative_scenario_tree.size = ${negativeScenarioTree.size}, " +
                "native_negative_tree.size = ${solver.context.get<NegativeScenarioTree>("nativeNegativeTree").size}")
        }

        // ==============
        // Verify automaton with NuSMV
        val counterexamples = automaton.verifyWithNuSMV(outDir)
        if (counterexamples.isEmpty()) {
            logger.info("CEGIS iteration #$iterationNumber done in %.3f s".format(timeSince(timeStart).seconds))
            logger.info("No counterexamples!")
            negativeScenarioTree.dump(outDir, "_negative_tree%d_iter%04d".format(loopNumber, iterationNumber), heat)
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
        if (solver.context.getOrNull<CutTreeMarker>("isCutTree") == null &&
            solver.context.getOrNull<heightMarker>("heightTree") == null &&
            solver.context.getOrNull<Cegisic>("cegisic") == null) {
            for (scenario in negativeScenarios) {
                negativeScenarioTree.addScenario(scenario)
            }
        } else {
            val nativeNegativeTree: NegativeScenarioTree = solver.context["nativeNegativeTree"]
            val length: Int? = solver.context.getOrNull("goodNodesCount")
                ?: solver.context.getOrNull<Int>("height")?.let { it + 1 }
                ?: solver.context.getOrNull<IncrementalCardinality>("negMappingCardinality")?.let { it.border }
            for (scenario in negativeScenarios) {
                nativeNegativeTree.addScenario(scenario)
                if (length != null) {
                    negativeScenarioTree.addScenario(scenario.subScenario(length))
                } else {
                    negativeScenarioTree.addScenario(scenario)
                }
            }
        }

        var curHeight = 0
        for (scenario in negativeScenarioTree.scenarios) {
            val path = automaton.eval(scenario).toList()
            for ((index, elements) in (scenario.elements zip path).withIndex()) {
                val (scenarioElement, pathElement) = elements
                if (scenarioElement.outputAction != pathElement.outputAction) {
                    break
                }
                heat.getOrPut(scenarioElement.nodeId!!) { mutableSetOf() } += iterationNumber
                curHeight = max(curHeight, index + 1)
            }
        }
        solver.context["curHeight"] = curHeight

        if (Globals.NEGATIVE_TREE_OPTIMIZATIONS == NegativeTreeOptimizations.OPT1 && iterationNumber > 50) {
            val needRegenerateNullVertex: Boolean = solver.context["needRegenerateNullVertex"]
            val doNotUseThisVertices: MutableSet<NegativeScenarioTree.Node> = solver.context["doNotUseThisVertices"]
            if (needRegenerateNullVertex) {
                solver.context("nullVertex") {
                    negativeScenarioTree.nodes
                        .associateWith { heat[it.id]?.size ?: 0 }
                        .filter { it.key !in doNotUseThisVertices }
                        .filter { it.value < iterationNumber * 0.95 }
                        .maxByOrNull { it.value }!!.key
                        .also { logger.info { "Null vertex = ${it.id}" } }
                }
                solver.context["needRegenerateNullVertex"] = false
            }
        }

        //if (iterationNumber % 20 == 0) {
        //    val nullVertex: NegativeScenarioTree.Node? = solver.context.getOrNull("nullVertex")
        //    negativeScenarioTree.dump(outDir, "_negative_tree%d_iter%04d".format(loopNumber, iterationNumber), heat, nullVertex?.id)
        //}

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
        logger.debug { "Dumping ${counterexamples.size} counterexample(s)..." }
        dir.resolve("ce").appendText(fileCounterexamples.readText())

        counterexamples
    } else {
        emptyList()
    }
}
