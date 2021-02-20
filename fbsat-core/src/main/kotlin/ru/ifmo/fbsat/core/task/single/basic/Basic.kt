package ru.ifmo.fbsat.core.task.single.basic

import com.soywiz.klock.PerformanceCounter
import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.buildBasicAutomaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.isSupportStats
import ru.ifmo.fbsat.core.solver.numberOfConflicts
import ru.ifmo.fbsat.core.solver.numberOfDecisions
import ru.ifmo.fbsat.core.solver.numberOfPropagations
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeT
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.timeSince

private val logger = MyLogger {}

fun Inferrer.basic(
    scenarioTree: PositiveScenarioTree,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true,
): Automaton? {
    val timeStart = PerformanceCounter.reference
    reset()
    declare(
        BasicTask(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = maxTransitions,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
    )
    val automaton = inferBasic()
    // logger.info("Task basic done in %.2f s".format(timeSince(timeStart).seconds))
    // if (solver.isSupportStats()) {
    //     logger.debug("Propagations: ${solver.numberOfPropagations()}")
    //     logger.debug("Conflicts: ${solver.numberOfConflicts()}")
    //     logger.debug("Decisions: ${solver.numberOfDecisions()}")
    // }
    if (Globals.IS_DUMP_CNF) {
        solver.dumpDimacs(outDir.resolve("cnf_basic_C${numberOfStates}_K${maxOutgoingTransitions}_T${maxTransitions}.cnf"))
    }
    return automaton
}

// TODO: return nullable Automaton instead of checking here.
fun Inferrer.basicMinC(
    scenarioTree: PositiveScenarioTree,
    start: Int = 1, // C_start
    end: Int = 20, // C_end
    isEncodeReverseImplication: Boolean = true,
): Automaton? {
    var best: Automaton? = null
    for (C in start..end) {
        val (result, runningTime) = measureTimeWithResult {
            basic(
                scenarioTree = scenarioTree,
                numberOfStates = C,
                isEncodeReverseImplication = isEncodeReverseImplication
            )
        }
        if (result != null) {
            logger.info("BasicMin: C = $C -> SAT in %.3f s.".format(runningTime.seconds))
            if (solver.isSupportStats()) {
                logger.debug("Propagations: ${solver.numberOfPropagations()}")
                logger.debug("Conflicts: ${solver.numberOfConflicts()}")
                logger.debug("Decisions: ${solver.numberOfDecisions()}")
            }
            logger.info("BasicMin: minimal C = $C")
            best = result
            break
        } else {
            logger.info("BasicMin: C = $C -> UNSAT in %.3f s.".format(runningTime.seconds))
            if (solver.isSupportStats()) {
                logger.debug("Propagations: ${solver.numberOfPropagations()}")
                logger.debug("Conflicts: ${solver.numberOfConflicts()}")
                logger.debug("Decisions: ${solver.numberOfDecisions()}")
            }
            if (isTimeout()) break
        }
    }
    return best
}

fun Inferrer.basicMin(
    scenarioTree: PositiveScenarioTree,
    start: Int = 1, // C_start
    end: Int = 20, // C_end
    isEncodeReverseImplication: Boolean = true,
): Automaton? {
    val timeStart = PerformanceCounter.reference
    val automaton = run {
        basicMinC(
            scenarioTree = scenarioTree,
            start = start,
            end = end,
            isEncodeReverseImplication = isEncodeReverseImplication
        ) ?: return@run null
        optimizeT()
    }
    logger.info("Task basic-min done in %.2f s".format(timeSince(timeStart).seconds))
    if (solver.isSupportStats()) {
        logger.debug("Propagations: ${solver.numberOfPropagations()}")
        logger.debug("Conflicts: ${solver.numberOfConflicts()}")
        logger.debug("Decisions: ${solver.numberOfDecisions()}")
    }
    if (Globals.IS_DUMP_CNF) {
        solver.dumpDimacs(outDir.resolve("cnf_basic-min.cnf"))
    }
    return automaton
}

fun Inferrer.inferBasic(): Automaton? {
    val model = solveAndGetModel() ?: return null
    val automaton = buildBasicAutomaton(solver.context, model)

    // TODO: refactor mapping check
    // with(vars) {
    //     check(
    //         automaton.checkMapping(
    //             scenarios = scenarioTree.scenarios,
    //             mapping = assignment.mapping
    //         )
    //     ) { "Positive mapping mismatch" }
    // }

    return automaton
}
