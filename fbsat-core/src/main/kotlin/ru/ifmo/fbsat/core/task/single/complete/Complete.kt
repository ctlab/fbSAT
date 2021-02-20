package ru.ifmo.fbsat.core.task.single.complete

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.isSupportStats
import ru.ifmo.fbsat.core.solver.numberOfConflicts
import ru.ifmo.fbsat.core.solver.numberOfDecisions
import ru.ifmo.fbsat.core.solver.numberOfPropagations
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeN
import ru.ifmo.fbsat.core.task.single.basic.BasicTask
import ru.ifmo.fbsat.core.task.single.basic.basicMinC
import ru.ifmo.fbsat.core.task.single.extended.ExtendedTask
import ru.ifmo.fbsat.core.task.single.extended.inferExtended
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.timeSince

private val logger = MyLogger {}

fun Inferrer.complete(
    scenarioTree: PositiveScenarioTree,
    negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
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
    declare(CompleteTask(negativeScenarioTree = negativeScenarioTree))
    val automaton = inferExtended()
    // logger.info("Task complete done in %.2f s".format(timeSince(timeStart).seconds))
    // if (solver.isSupportStats()) {
    //     logger.debug("Propagations: ${solver.numberOfPropagations()}")
    //     logger.debug("Conflicts: ${solver.numberOfConflicts()}")
    //     logger.debug("Decisions: ${solver.numberOfDecisions()}")
    // }
    if (Globals.IS_DUMP_CNF) {
        solver.dumpDimacs(outDir.resolve("cnf_complete_C${numberOfStates}_K${maxOutgoingTransitions}_P${maxGuardSize}_T${maxTransitions}_N${maxTotalGuardsSize}.cnf"))
    }
    return automaton
}

fun Inferrer.completeMin(
    scenarioTree: PositiveScenarioTree,
    negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
    maxGuardSize: Int, // P
): Automaton? {
    val timeStart = PerformanceCounter.reference
    val automaton = run {
        val basicAutomaton = basicMinC(scenarioTree) ?: return@run null
        // Note: we have to reset because basicMinC uses isEncodeReverseImplication = true,
        //  which is incompatible with negative reduction
        // TODO: check whether isEncodeReverseImplication is actually true in current basicVars
        reset()
        declare(
            BasicTask(
                scenarioTree = scenarioTree,
                numberOfStates = basicAutomaton.numberOfStates,
                isEncodeReverseImplication = false
            )
        )
        declare(ExtendedTask(maxGuardSize = maxGuardSize))
        declare(CompleteTask(negativeScenarioTree))
        optimizeN()
    }
    logger.info("Task complete-min done in %.2f s".format(timeSince(timeStart).seconds))
    if (solver.isSupportStats()) {
        logger.debug("Propagations: ${solver.numberOfPropagations()}")
        logger.debug("Conflicts: ${solver.numberOfConflicts()}")
        logger.debug("Decisions: ${solver.numberOfDecisions()}")
    }
    if (Globals.IS_DUMP_CNF) {
        solver.dumpDimacs(outDir.resolve("cnf_complete-min_P${maxGuardSize}.cnf"))
    }
    return automaton
}

fun Inferrer.completeMinUB(): Automaton? {
    TODO("completeMinUB")
}
