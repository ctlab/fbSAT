package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveConsecutiveModularMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.task.single.CONSECUTIVE_MODULAR_BASIC_VARS
import ru.ifmo.fbsat.core.task.single.consecutiveModularBasicVars
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

fun Solver.declareConsecutiveModularBasic(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
) {
    require(numberOfModules >= 2) { "Number of modules must be at least 2" }
    require(scenarioTree.inputEvents == listOf(InputEvent("REQ")))
    require(scenarioTree.outputEvents == listOf(OutputEvent("CNF")))

    val timeStart = PerformanceCounter.reference
    val nvarStart = numberOfVariables
    val nconStart = numberOfClauses

    /* Variables */
    val vars = declareConsecutiveModularBasicVariables(
        scenarioTree = scenarioTree,
        M = numberOfModules,
        C = numberOfStates,
        K = maxOutgoingTransitions ?: numberOfStates
    ).also {
        context[CONSECUTIVE_MODULAR_BASIC_VARS] = it
    }

    /* Constraints */
    declareConsecutiveModularAutomatonStructureConstraints(vars)
    if (Globals.IS_BFS_AUTOMATON) declareConsecutiveModularAutomatonBfsConstraints(vars)
    declarePositiveConsecutiveModularMappingConstraints(vars, isEncodeReverseImplication)
    declareConsecutiveModularBasicAdhocConstraints()

    /* Initial cardinality constraints */
    vars.cardinality.updateUpperBoundLessThanOrEqual(maxTransitions)

    val nvarDiff = numberOfVariables - nvarStart
    val nconDiff = numberOfClauses - nconStart
    log.info(
        "Done declaring consecutive modular basic variables ($nvarDiff) and constraints ($nconDiff) in %.2f s."
            .format(timeSince(timeStart).seconds)
    )
}

private fun Solver.declareConsecutiveModularBasicAdhocConstraints() {
    val vars = consecutiveModularBasicVars

    comment("ADHOC constraints")
    with(vars) {
        comment("Ad-hoc: no transition to the first state")
        for (m in 1..M) with(modularBasicVariables[m]) {
            for (c in 1..C)
                for (k in 1..K)
                    clause(transitionDestination[c, k] neq 1)
        }
    }
}
