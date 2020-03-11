package ru.ifmo.fbsat.core.task.modular.basic.parallel

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.ParallelModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeTopDown
import ru.ifmo.fbsat.core.task.parallelModularBasicVars
import ru.ifmo.fbsat.core.utils.log

fun Inferrer.inferParallelModularBasic(): ParallelModularAutomaton? {
    val rawAssignment = solver.solve() ?: return null
    val vars = solver.parallelModularBasicVars
    val assignment = ParallelModularBasicAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    // TODO: check automaton
    check(true)

    return automaton
}

fun Inferrer.optimizeParallelModularT(start: Int? = null, end: Int = 0): ParallelModularAutomaton? {
    log.info("Optimizing T...")
    val vars = solver.parallelModularBasicVars
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { T ->
            vars.cardinality.updateUpperBoundLessThanOrEqual(T)
            inferParallelModularBasic()
        },
        next = { T ->
            vars.cardinality.updateUpperBoundLessThan(T)
            inferParallelModularBasic()
        },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.parallelModularBasic(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
): ParallelModularAutomaton? {
    reset()
    solver.declareParallelModularBasic(
        scenarioTree = scenarioTree,
        numberOfModules = numberOfModules,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions,
        maxTransitions = maxTransitions,
        isEncodeReverseImplication = isEncodeReverseImplication
    )
    return inferParallelModularBasic()
}

fun Inferrer.parallelModularBasicMinC(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    start: Int = 1, // C_start
    end: Int = 20 // C_end
): ParallelModularAutomaton {
    var best: ParallelModularAutomaton? = null
    for (C in start..end) {
        reset()
        solver.declareParallelModularBasic(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = C
        )
        val (result, runningTime) = measureTimeWithResult { inferParallelModularBasic() }
        if (result != null) {
            log.success("ParallelModularBasicMin: C = $C -> SAT in %.3f s.".format(runningTime.seconds))
            log.info("ParallelModularBasicMin: minimal C = $C")
            best = result
            break
        } else {
            log.failure("ParallelModularBasicMin: C = $C -> UNSAT in %.3f s.".format(runningTime.seconds))
        }
    }
    return checkNotNull(best) { "ParallelModularBasicMin: automaton not found." }
}

fun Inferrer.parallelModularBasicMin(
    scenarioTree: ScenarioTree,
    numberOfModules: Int // M
): ParallelModularAutomaton? {
    parallelModularBasicMinC(scenarioTree, numberOfModules = numberOfModules)
    return optimizeParallelModularT()
}
