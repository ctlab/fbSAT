@file:JvmName("BasicTaskKt")

package ru.ifmo.fbsat.core.task.single.basic

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.buildBasicAutomaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeT
import ru.ifmo.fbsat.core.utils.log

fun Inferrer.basic(
    scenarioTree: PositiveScenarioTree,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true
): Automaton? {
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
    return inferBasic()
}

// TODO: return nullable Automaton instead of checking here.
fun Inferrer.basicMinC(
    scenarioTree: PositiveScenarioTree,
    start: Int = 1, // C_start
    end: Int = 20, // C_end
    isEncodeReverseImplication: Boolean = true
): Automaton {
    var best: Automaton? = null
    for (C in start..end) {
        val (result, runningTime) = measureTimeWithResult {
            basic(scenarioTree, numberOfStates = C, isEncodeReverseImplication = isEncodeReverseImplication)
        }
        if (result != null) {
            log.success("BasicMin: C = $C -> SAT in %.3f s.".format(runningTime.seconds))
            log.info("BasicMin: minimal C = $C")
            best = result
            break
        } else {
            log.failure("BasicMin: C = $C -> UNSAT in %.3f s.".format(runningTime.seconds))
        }
    }
    return checkNotNull(best) { "BasicMin: automaton not found." }
}

fun Inferrer.basicMin(
    scenarioTree: PositiveScenarioTree,
    isEncodeReverseImplication: Boolean = true
): Automaton? {
    basicMinC(scenarioTree, isEncodeReverseImplication = isEncodeReverseImplication)
    return optimizeT()
}

fun Inferrer.inferBasic(): Automaton? {
    val rawAssignment = solver.solve() ?: return null
    // val vars = solver.context.basicVars
    // val assignment = BasicAssignment.fromRaw(rawAssignment, vars)
    // val automaton = assignment.toAutomaton()
    val automaton = buildBasicAutomaton(
        context = solver.context,
        raw = rawAssignment
    )

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
