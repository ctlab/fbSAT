package ru.ifmo.fbsat.core.task.modular.basic.parallel

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.ParallelModularAutomaton
import ru.ifmo.fbsat.core.automaton.buildBasicParallelModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.solveAndGetModel
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeParallelModularT
import ru.ifmo.fbsat.core.utils.MyLogger

private val logger = MyLogger {}

fun Inferrer.parallelModularBasic(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true,
): ParallelModularAutomaton? {
    reset()
    declare(
        ParallelModularBasicTask(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = maxTransitions,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
    )
    return inferParallelModularBasic()
}

fun Inferrer.parallelModularBasicMinC(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    start: Int = 1, // C_start
    end: Int = 20, // C_end
): ParallelModularAutomaton {
    var best: ParallelModularAutomaton? = null
    for (C in start..end) {
        val (result, runningTime) = measureTimeWithResult {
            parallelModularBasic(
                scenarioTree = scenarioTree,
                numberOfModules = numberOfModules,
                numberOfStates = C
            )
        }
        if (result != null) {
            logger.info("ParallelModularBasicMin: C = $C -> SAT in %.3f s.".format(runningTime.seconds))
            logger.info("ParallelModularBasicMin: minimal C = $C")
            best = result
            break
        } else {
            logger.info("ParallelModularBasicMin: C = $C -> UNSAT in %.3f s.".format(runningTime.seconds))
        }
    }
    return checkNotNull(best) { "ParallelModularBasicMin: automaton not found." }
}

fun Inferrer.parallelModularBasicMin(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int? = null, // C_start, 1 if null
): ParallelModularAutomaton? {
    parallelModularBasicMinC(scenarioTree, numberOfModules = numberOfModules, start = numberOfStates ?: 1)
    return optimizeParallelModularT()
}

fun Inferrer.inferParallelModularBasic(): ParallelModularAutomaton? {
    val model = solver.solveAndGetModel() ?: return null
    val automaton = buildBasicParallelModularAutomaton(solver.context, model)

    // TODO: check automaton
    // log.warn("Mapping check is not implemented yet")

    return automaton
}
