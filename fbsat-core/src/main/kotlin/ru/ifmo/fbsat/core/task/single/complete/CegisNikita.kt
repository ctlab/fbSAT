package ru.ifmo.fbsat.core.task.single.complete

import com.github.lipen.satlib.card.Cardinality
import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.Lit
import com.github.lipen.satlib.core.newIntVar
import com.github.lipen.satlib.op.imply
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.automaton.Automaton
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
import kotlin.system.exitProcess

private val logger = MyLogger {}

fun Inferrer.cegisNikita(
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
    solver.context["chunk"] = 100
    declare(ExtendedTask(maxGuardSize = maxGuardSize, maxTotalGuardsSize = maxTotalGuardsSize))
    declare(CompleteTask(negativeScenarioTree))
    return performCegis(smvDir, loopNumber)
}

fun Solver.unregisterMaxNegVerticesSize() {
    context.getOrNull<() -> List<Lit>>("goodNodesCard")?.let { assumptionsObservable.unregister(it) }
}

fun Inferrer.optimizeMaxNegVerticesSize(increasedN: Boolean): Automaton? {
    val zeroTotalizer: List<Lit> = solver.context["zeroTotalizer"]
    val chunk: Int = solver.context["chunk"]
    val negativeScenarioTree: NegativeScenarioTree = solver.context["negativeScenarioTree"]
    val heat: MutableMap<Int, MutableSet<Int>> = solver.context["heat"]
    val query = { it: Automaton ->
        val set = mutableSetOf<Int>()
        //negativeScenarioTree.dump(outDir, "bad_negative_tree")
        //it.dump(outDir, "bad_automaton")
        for (scenario in negativeScenarioTree.scenarios) {
            val path = it.eval(scenario).toList()
            for ((scenarioElement, pathElement) in scenario.elements zip path) {
                //println(Pair(scenarioElement.nodeId!!, pathElement.destination.id))
                if (scenarioElement.outputAction != pathElement.outputAction) {
                    break
                }
                set += scenarioElement.nodeId!!
            }
        }
        set.size + 1 // one for root
    }
    val automaton = optimizeTopDown(
        start = null,
        end = if (increasedN) 1 else solver.context.get<Int>("goodNodesCount") + 1,
        nextInitial = {
            val goodNodesCard = solver.context("goodNodesCard") {
                { listOf(-zeroTotalizer[chunk - 1]) }
            }
            solver.assumptionsObservable.register(goodNodesCard)
            inferExtended()
        },
        next = {
            logger.error("it=$it")
            solver.unregisterMaxNegVerticesSize()
            val goodNodesCard = solver.context("goodNodesCard") {
                { listOf(-zeroTotalizer[it - 1]) }
            }
            solver.assumptionsObservable.register(goodNodesCard)
            inferExtended()
        },
        query = query
    ) ?: return null
    solver.unregisterMaxNegVerticesSize()
    val goodNodesCount = solver.context("goodNodesCount") { query(automaton) }
    //println("Good nodes count = $goodNodesCount")
    val goodNodesCard = solver.context("goodNodesCard") { { listOf(-zeroTotalizer[goodNodesCount]) } }
    solver.assumptionsObservable.register(goodNodesCard)
    //check(inferExtended() != null) { "Something went wrong" }
    return automaton
}

fun Inferrer.cegisNikitaStep(
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
    val lastAutomaton = optimizeMaxNegVerticesSize(wasNIncrease)
    if (lastAutomaton == null) {
        logger.error("Probably too small chunk size = ${solver.context.get<Int>("chunk")}")
        exitProcess(0)
    }
    return performCegis(smvDir, loopNumber)
}

@Suppress("LocalVariableName")
fun Inferrer.cegisMinNikita(
    scenarioTree: PositiveScenarioTree,
    initialNegativeScenarioTree: NegativeScenarioTree? = null,
    numberOfStates: Int? = null,
    startNumberOfStates: Int? = null,
    maxGuardSize: Int? = null, // P, search if null
    maxPlateauWidth: Int? = null, // w, =Inf if null
    smvDir: File,
): Automaton? {
    check(Globals.IS_USE_ASSUMPTIONS) { "Currently, cegis-min-nikita only works with assumptions being turned on globally. Pass --use-assumptions flag to enable them." }

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
            cegisNikita(
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
            cegisNikitaStep(N, smvDir, loopNumber, wasNIncrease)
        }

        if (automaton != null) {
            logger.info("Hooray! Minimal full verified automaton has been found!")
            return automaton
        } else {
            logger.info("UNSAT, N = $N is too small, trying to increase...")
            solver.unregisterMaxNegVerticesSize()
            val prevN = N
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
