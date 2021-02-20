package ru.ifmo.fbsat.core.task.single.complete

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.AssumptionSupportable
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeN
import ru.ifmo.fbsat.core.task.optimizeTopDown
import ru.ifmo.fbsat.core.task.single.basic.BasicTask
import ru.ifmo.fbsat.core.task.single.extended.ExtendedTask
import ru.ifmo.fbsat.core.task.single.extended.extendedMin
import ru.ifmo.fbsat.core.task.single.extended.extendedMinUB
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import java.io.File

fun Inferrer.cegisAssumptions(
    scenarioTree: PositiveScenarioTree,
    negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    smvDir: File,
): Automaton? {
    check(solver is AssumptionSupportable) { "Solver doesn't support assumptions" }
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
    declare(ExtendedTask(maxGuardSize = maxGuardSize, maxTotalGuardsSize = maxTotalGuardsSize, useAssumptions = true))
    declare(CompleteTask(negativeScenarioTree))
    return performCegis(smvDir)
}

fun Inferrer.cegisAssumptionsStep(
    maxTotalGuardsSize: Int,
    smvDir: File,
): Automaton? {
    check(solver is AssumptionSupportable) { "Solver doesn't support assumptions" }
    solver.apply {
        // TODO: support reset_assumptions in Cadical solver or we need to check that Cadical's last result is UNSAT
        clearAssumptions()
        val cardinalityN: Cardinality = context["cardinalityN"]
        cardinalityN.updateUpperBoundLessThanOrEqual(maxTotalGuardsSize)
    }
    return performCegis(smvDir)
}

@Suppress("LocalVariableName")
fun Inferrer.cegisAssumptionsMin(
    scenarioTree: PositiveScenarioTree,
    initialNegativeScenarioTree: NegativeScenarioTree? = null,
    numberOfStates: Int? = null,
    startNumberOfStates: Int? = null,
    maxGuardSize: Int? = null, // P, search if null
    maxPlateauWidth: Int? = null, // w, =Inf if null
    smvDir: File,
): Automaton? {
    check(Globals.USE_ASSUMPTIONS) { "Permit using assumptions" }

    val extendedAutomaton = if (maxGuardSize == null) {
        extendedMinUB(scenarioTree, numberOfStates = startNumberOfStates, maxPlateauWidth = maxPlateauWidth)
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

    val negativeScenarioTree = initialNegativeScenarioTree ?: NegativeScenarioTree(scenarioTree)

    for (loopNumber in 1..100) {
        log.just("===== Loop number #$loopNumber, N = $N =====")

        val automaton = if (loopNumber == 1) cegisAssumptions(
            scenarioTree = scenarioTree,
            negativeScenarioTree = negativeScenarioTree,
            numberOfStates = C,
            maxOutgoingTransitions = K,
            maxGuardSize = P,
            maxTotalGuardsSize = N,
            smvDir = smvDir,
        ) else cegisAssumptionsStep(N, smvDir)

        if (automaton != null) {
            log.just("Hooray! Minimal full verified automaton has been found!")
            return automaton
        } else {
            log.just("UNSAT, N = $N is too small, trying larger value...")
            check(solver is AssumptionSupportable) { "Solver doesn't support assumptions" }
            log.info("Assumptions-cegis-step")
            solver.clearAssumptions()
            val curAutomaton = optimizeN(start = C * K * P, end = N + 1)
            if (curAutomaton == null) {
                log.error("N reached upper bound C*K*P = ${C * K * P}")
                break
            }
            N = curAutomaton.totalGuardsSize
        }
    }
    return null
}
