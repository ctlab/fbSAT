package ru.ifmo.fbsat.core.task.single.complete

import com.github.lipen.satlib.card.Cardinality
import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.Lit
import com.github.lipen.satlib.core.newIntVar
import com.github.lipen.satlib.op.imply
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeN
import ru.ifmo.fbsat.core.task.optimizeTopDown
import ru.ifmo.fbsat.core.task.single.basic.BasicTask
import ru.ifmo.fbsat.core.task.single.extended.ExtendedTask
import ru.ifmo.fbsat.core.task.single.extended.extendedMin
import ru.ifmo.fbsat.core.task.single.extended.extendedMinUB
import ru.ifmo.fbsat.core.task.single.extended.inferExtended
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import java.io.File
import kotlin.math.max
import kotlin.system.exitProcess

private val logger = MyLogger {}

object heightMarker

fun Inferrer.cegisHeight(
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
    solver.context["heightTree"] = heightMarker
    if (negativeScenarioTree != null) {
        solver.context["nativeNegativeTree"] = NegativeScenarioTree(
            negativeScenarioTree.inputEvents,
            negativeScenarioTree.outputEvents,
            negativeScenarioTree.inputNames,
            negativeScenarioTree.outputNames,
            negativeScenarioTree.isTrie,
        )
    }
    declare(ExtendedTask(maxGuardSize = maxGuardSize, maxTotalGuardsSize = maxTotalGuardsSize))
    declare(CompleteTask(negativeScenarioTree))
    return performCegis(smvDir, loopNumber)
}

fun Solver.unregisterHeight() {
    context.getOrNull<() -> List<Lit>>("heightConstraint")?.let { assumptionsObservable.unregister(it) }
}

fun Inferrer.optimizeHeight(increasedN: Boolean): Automaton? {
    val heightTotalizer: MutableList<Lit> = solver.context["heightTotalizer"]
    val maxHeight: Int = solver.context["maxHeight"]
    val negativeScenarioTree: NegativeScenarioTree = solver.context["negativeScenarioTree"]
    val query = { it: Automaton ->
        var height = 0
        //negativeScenarioTree.dump(outDir, "bad_negative_tree")
        //it.dump(outDir, "bad_automaton")
        for (scenario in negativeScenarioTree.scenarios) {
            val path = it.eval(scenario).toList()
            for ((index, elements) in (scenario.elements zip path).withIndex()) {
                val (scenarioElement, pathElement) = elements
                //println(Pair(scenarioElement.nodeId!!, pathElement.destination.id))
                if (scenarioElement.outputAction != pathElement.outputAction) {
                    break
                }
                height = max(height, index + 1)
            }
        }
        height
    }
    val automaton = optimizeTopDown(
        start = null,
        end = if (increasedN) 0 else solver.context.get<Int>("height") + 1,
        nextInitial = {
            inferExtended().also {
                solver.assumptionsObservable.register(solver.context("heightConstraint") {
                    { listOf() }
                })
            }
        },
        next = {
            solver.unregisterHeight()
            val heightConstraint = solver.context("heightConstraint") {
                { listOf(-heightTotalizer[it - 1]) }
            }
            solver.assumptionsObservable.register(heightConstraint)
            inferExtended()
        },
        query = query
    ) ?: return null
    solver.unregisterHeight()
    val height = solver.context("height") { query(automaton) }
    solver.context["curHeight"] = height
    //println("Good nodes count = $goodNodesCount")
    if (maxHeight <= height) {
        solver.apply {
            for (i in maxHeight until height) {
                heightTotalizer += newLiteral()
            }
        }
        solver.context["maxHeight"] = height
    }
    //println("maxHeight = ${solver.context.get<Int>("maxHeight")}, totalizerLength = ${heightTotalizer.size}")
    val heightConstraint = solver.context("heightConstraint") { { listOf(-heightTotalizer[height]) } }
    solver.assumptionsObservable.register(heightConstraint)
    return automaton
}

fun Inferrer.cegisHeightStep(
    maxTotalGuardsSize: Int,
    smvDir: File,
    loopNumber: Int,// = 0,
    wasNIncrease: Boolean,
): Automaton? {
    solver.apply {
        // clearAssumptions()
        val cardinalityN: Cardinality = context["cardinalityN"]
        cardinalityN.assumeUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
    val oldHeight: Int = solver.context["height"]
    val lastAutomaton = optimizeHeight(wasNIncrease)
        ?: error("Something went very wrong")
    val height: Int = solver.context["height"]
    if (height > oldHeight) {
        val negativeScenarioTree: NegativeScenarioTree = solver.context["negativeScenarioTree"]
        val nativeNegativeTree: NegativeScenarioTree = solver.context["nativeNegativeTree"]
        for (scenario in nativeNegativeTree.scenarios) {
            negativeScenarioTree.addScenario(scenario.subScenario(height + 1))
        }
    }
    return performCegis(smvDir, loopNumber)
}

@Suppress("LocalVariableName")
fun Inferrer.cegisMinHeight(
    scenarioTree: PositiveScenarioTree,
    initialNegativeScenarioTree: NegativeScenarioTree? = null,
    numberOfStates: Int? = null,
    startNumberOfStates: Int? = null,
    maxGuardSize: Int? = null, // P, search if null
    maxPlateauWidth: Int? = null, // w, =Inf if null
    smvDir: File,
): Automaton? {
    check(Globals.IS_USE_ASSUMPTIONS) { "Currently, height only works with assumptions being turned on globally. Pass --use-assumptions flag to enable them." }
    check(Globals.LOOPLESS_COUNTER_EXAMPLES) { "Currently, height only works with enabled loopless counter-examples. Pass --loopless-counter-examples flag to enable them." }

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

    var wasNIncrease = false

    for (loopNumber in 1..100) {
        logger.just("===== Loop number #$loopNumber, N = $N =====")

        val automaton = if (loopNumber == 1) {
            cegisHeight(
                scenarioTree = scenarioTree,
                negativeScenarioTree = negativeScenarioTree,
                numberOfStates = C,
                maxOutgoingTransitions = K,
                maxGuardSize = P,
                maxTotalGuardsSize = N,
                smvDir = smvDir,
                loopNumber = loopNumber
            )
        } else {
            cegisHeightStep(N, smvDir, loopNumber, wasNIncrease)
        }

        if (automaton != null) {
            logger.info("Hooray! Minimal full verified automaton has been found!")
            return automaton
        } else {
            logger.info("UNSAT, N = $N is too small, trying to increase...")
            solver.unregisterHeight()
            val curAutomaton = optimizeN(start = null, end = N)
            if (curAutomaton == null) {
                logger.error("Automaton not found even without an upper bound on N")
                break
            } else {
                wasNIncrease = curAutomaton.totalGuardsSize != N
                N = curAutomaton.totalGuardsSize
                logger.info("Found new suitable minimal N = $N")
            }
        }
    }
    return null
}

