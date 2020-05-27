package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.constraints.declareArbitraryModularAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareArbitraryModularAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveArbitraryModularMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.VARS
import ru.ifmo.fbsat.core.task.arbitraryModularBasicVars
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

fun Solver.declareArbitraryModularBasic(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
) {
    val timeStart = PerformanceCounter.reference
    val nvarStart = numberOfVariables
    val nconStart = numberOfClauses

    /* Variables */
    val vars = declareArbitraryModularBasicVariables(
        scenarioTree = scenarioTree,
        M = numberOfModules,
        C = numberOfStates,
        K = maxOutgoingTransitions ?: numberOfStates
    ).also {
        context[VARS.ARBITRARY_MODULAR_BASIC] = it
    }

    /* Constraints */
    declareArbitraryModularAutomatonStructureConstraints(vars)
    if (Globals.IS_BFS_AUTOMATON) declareArbitraryModularAutomatonBfsConstraints(vars)
    declarePositiveArbitraryModularMappingConstraints(vars, isEncodeReverseImplication)
    declareArbitraryModularBasicAdhocConstraints()

    /* Initial cardinality constraints */
    vars.cardinality.updateUpperBoundLessThanOrEqual(maxTransitions)

    val nvarDiff = numberOfVariables - nvarStart
    val nconDiff = numberOfClauses - nconStart
    log.info(
        "Done declaring arbitrary modular basic variables ($nvarDiff) and constraints ($nconDiff) in %.3f s."
            .format(timeSince(timeStart).seconds)
    )
}

private fun Solver.declareArbitraryModularBasicAdhocConstraints() {
    val vars = arbitraryModularBasicVars
    // comment("ADHOC constraints")
}
