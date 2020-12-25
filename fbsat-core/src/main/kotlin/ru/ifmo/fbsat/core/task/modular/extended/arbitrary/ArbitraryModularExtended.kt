package ru.ifmo.fbsat.core.task.modular.extended.arbitrary

import ru.ifmo.fbsat.core.automaton.ArbitraryModularAutomaton
import ru.ifmo.fbsat.core.automaton.buildExtendedArbitraryModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.solveAndGetModel
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.ArbitraryModularBasicTask
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.arbitraryModularBasicMinC
import ru.ifmo.fbsat.core.task.optimizeArbitraryModularN

fun Inferrer.arbitraryModularExtended(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    isEncodeReverseImplication: Boolean = true,
): ArbitraryModularAutomaton? {
    reset()
    declare(
        ArbitraryModularBasicTask(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = maxTransitions,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
    )
    declare(
        ArbitraryModularExtendedTask(
            maxGuardSize = maxGuardSize,
            maxTotalGuardsSize = maxTotalGuardsSize
        )
    )
    solver.dumpDimacs(outDir.resolve("cnf_modular-arbitrary-extended_M${numberOfModules}_C${numberOfStates}.cnf"))
    return inferArbitraryModularExtended()
}

fun Inferrer.arbitraryModularExtendedMin(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int? = null, // C_start, 1 if null
    maxGuardSize: Int, // P
): ArbitraryModularAutomaton? {
    arbitraryModularBasicMinC(scenarioTree, numberOfModules, start = numberOfStates ?: 1)
    declare(
        ArbitraryModularExtendedTask(
            maxGuardSize = maxGuardSize
        )
    )
    return optimizeArbitraryModularN()
}

fun Inferrer.inferArbitraryModularExtended(): ArbitraryModularAutomaton? {
    val model = solver.solveAndGetModel() ?: return null
    val automaton = buildExtendedArbitraryModularAutomaton(solver.context, model)

    // TODO: check automaton
    // log.warn("Mapping check is not implemented yet")

    return automaton
}
