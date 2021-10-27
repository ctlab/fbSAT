package ru.ifmo.fbsat.core.task.single.extended

import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.convert
import com.github.lipen.satlib.core.sign
import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.buildExtendedAutomaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.isSupportStats
import ru.ifmo.fbsat.core.solver.numberOfConflicts
import ru.ifmo.fbsat.core.solver.numberOfDecisions
import ru.ifmo.fbsat.core.solver.numberOfPropagations
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeN
import ru.ifmo.fbsat.core.task.single.basic.BasicTask
import ru.ifmo.fbsat.core.task.single.basic.basic
import ru.ifmo.fbsat.core.task.single.basic.basicMin
import ru.ifmo.fbsat.core.task.single.basic.basicMinC
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.timeSince

private val logger = MyLogger {}

fun Inferrer.extended(
    scenarioTree: PositiveScenarioTree,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    isEncodeReverseImplication: Boolean = true,
): Automaton? {
    // val timeStart = PerformanceCounter.reference
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
    declare(
        ExtendedTask(
            maxGuardSize = maxGuardSize,
            maxTotalGuardsSize = maxTotalGuardsSize
        )
    )
    val automaton = inferExtended()
    // logger.info("Task extended done in %.2f s".format(timeSince(timeStart).seconds))
    // if (solver.isSupportStats()) {
    //     logger.debug("Propagations: ${solver.numberOfPropagations()}")
    //     logger.debug("Conflicts: ${solver.numberOfConflicts()}")
    //     logger.debug("Decisions: ${solver.numberOfDecisions()}")
    // }
    if (Globals.IS_DUMP_CNF) {
        solver.dumpDimacs(outDir.resolve("cnf_extended_C${numberOfStates}_K${maxOutgoingTransitions}_P${maxGuardSize}_T${maxTransitions}_N${maxTotalGuardsSize}.cnf"))
    }
    return automaton
}

fun Inferrer.extendedMin(
    scenarioTree: PositiveScenarioTree,
    numberOfStates: Int? = null, // C_start
    maxOutgoingTransitionsForC: (Int) -> Int? = { null }, // lambda for computing K for C (input arg)
    maxGuardSize: Int, // P
): Automaton? {
    val timeStart = PerformanceCounter.reference
    val automaton = run {
        basicMinC(
            scenarioTree,
            start = numberOfStates ?: 1,
            maxOutgoingTransitionsForC = maxOutgoingTransitionsForC,
        ) ?: return@run null
        if (Globals.IS_ENCODE_EVENTLESS) {
            if (Globals.IS_FIX_ACTIVE) {
                logger.info("Fixing found active[v] values")
                @Suppress("LocalVariableName")
                with(solver) {
                    val V: Int = context["V"]
                    val active: BoolVarArray = context["active"]
                    val model = getModel()
                    val activeVal = active.convert(model)
                    for (v in 1..V) {
                        clause(active[v] sign activeVal[v])
                    }
                }
            }
        }
        declare(ExtendedTask(maxGuardSize = maxGuardSize))
        optimizeN()
    }
    logger.info("Task extended-min done in %.2f s".format(timeSince(timeStart).seconds))
    if (solver.isSupportStats()) {
        logger.debug("Propagations: ${solver.numberOfPropagations()}")
        logger.debug("Conflicts: ${solver.numberOfConflicts()}")
        logger.debug("Decisions: ${solver.numberOfDecisions()}")
    }
    if (Globals.IS_DUMP_CNF) {
        solver.dumpDimacs(outDir.resolve("cnf_extended-min_P${maxGuardSize}.cnf"))
    }
    return automaton
}

@Suppress("LocalVariableName")
fun Inferrer.extendedMinUB(
    scenarioTree: PositiveScenarioTree,
    numberOfStates: Int? = null, // C_start
    start: Int = 1, // P_start
    end: Int = 20, // P_end
    maxPlateauWidth: Int? = null, // w, =Inf if null
): Automaton? {
    require(start >= 1)
    require(end >= 1)
    require(start <= end)

    val timeStart = PerformanceCounter.reference

    val answer = run {
        val basicAutomaton =
            if (numberOfStates != null) {
                basic(scenarioTree, numberOfStates)
            } else {
                basicMin(scenarioTree)
            } ?: return@run null
        val C = basicAutomaton.numberOfStates
        val Tmin = basicAutomaton.numberOfTransitions
        var best: Automaton? = null
        var Plow: Int? = null
        var N: Int? = null // <=

        logger.info("Tmin = $Tmin")

        for (P in start..end) {
            logger.info("Trying P = $P, N = $N")

            if (best != null && P > (best.totalGuardsSize - Tmin)) {
                logger.info("Reached upper bound: P = $P, Plow = $Plow, Nbest = ${best.totalGuardsSize}, Tmin = $Tmin")
                break
            }
            if (Plow != null && maxPlateauWidth != null && (P - Plow) > maxPlateauWidth) {
                logger.info("Reached maximum plateau width: P = $P, Plow = $Plow, w = $maxPlateauWidth")
                break
            }

            reset()
            declare(BasicTask(scenarioTree, numberOfStates = C))
            declare(ExtendedTask(maxGuardSize = P))
            val automaton = optimizeN(start = N)
            if (automaton != null) {
                best = automaton
                Plow = P
                N = automaton.totalGuardsSize - 1
            }
        }

        best
    }

    logger.info("Task extended-min-ub done in %.2f s".format(timeSince(timeStart).seconds))
    if (solver.isSupportStats()) {
        logger.debug("Propagations: ${solver.numberOfPropagations()}")
        logger.debug("Conflicts: ${solver.numberOfConflicts()}")
        logger.debug("Decisions: ${solver.numberOfDecisions()}")
    }
    if (Globals.IS_DUMP_CNF) {
        solver.dumpDimacs(outDir.resolve("cnf_extended-min-ub_w${maxPlateauWidth}.cnf"))
    }

    return answer
}

fun Inferrer.inferExtended(): Automaton? {
    val model = solveAndGetModel() ?: return null
    val automaton = buildExtendedAutomaton(solver.context, model)

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
