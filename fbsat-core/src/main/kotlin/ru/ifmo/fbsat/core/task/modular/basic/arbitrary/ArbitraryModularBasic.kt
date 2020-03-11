package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.ArbitraryModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.arbitraryModularBasicVars
import ru.ifmo.fbsat.core.task.optimizeTopDown
import ru.ifmo.fbsat.core.utils.log

fun Inferrer.inferArbitraryModularBasic(): ArbitraryModularAutomaton? {
    val rawAssignment = solver.solve() ?: return null
    val vars = solver.arbitraryModularBasicVars
    val assignment = ArbitraryModularBasicAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    // TODO: check automaton
    check(true)

    return automaton
}

fun Inferrer.optimizeArbitraryModularT(start: Int? = null, end: Int = 0): ArbitraryModularAutomaton? {
    log.info("Optimizing T...")
    val vars = solver.arbitraryModularBasicVars
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { T ->
            vars.cardinality.updateUpperBoundLessThanOrEqual(T)
            inferArbitraryModularBasic()
        },
        next = { T ->
            vars.cardinality.updateUpperBoundLessThan(T)
            inferArbitraryModularBasic()
        },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.arbitraryModularBasic(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
): ArbitraryModularAutomaton? {
    reset()
    solver.declareArbitraryModularBasic(
        scenarioTree = scenarioTree,
        numberOfModules = numberOfModules,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions,
        maxTransitions = maxTransitions,
        isEncodeReverseImplication = isEncodeReverseImplication
    )
    return inferArbitraryModularBasic()
}

fun Inferrer.arbitraryModularBasicMinC(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    start: Int = 1, // C_start
    end: Int = 20 // C_end
): ArbitraryModularAutomaton {
    var best: ArbitraryModularAutomaton? = null
    for (C in start..end) {
        reset()
        solver.declareArbitraryModularBasic(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = C
        )
        val (result, runningTime) = measureTimeWithResult { inferArbitraryModularBasic() }
        if (result != null) {
            log.success("ArbitraryModularBasicMin: C = $C -> SAT in %.3f s.".format(runningTime.seconds))
            log.info("ArbitraryModularBasicMin: minimal C = $C")
            best = result
            break
        } else {
            log.failure("ArbitraryModularBasicMin: C = $C -> UNSAT in %.3f s.".format(runningTime.seconds))
        }
    }
    return checkNotNull(best) { "ArbitraryModularBasicMin: automaton not found." }
}

fun Inferrer.arbitraryModularBasicMin(
    scenarioTree: ScenarioTree,
    numberOfModules: Int // M
): ArbitraryModularAutomaton? {
    arbitraryModularBasicMinC(scenarioTree, numberOfModules = numberOfModules)
    return optimizeArbitraryModularT()
}
