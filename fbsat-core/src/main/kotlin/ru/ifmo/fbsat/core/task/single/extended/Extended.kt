package ru.ifmo.fbsat.core.task.single.extended

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.extendedVars
import ru.ifmo.fbsat.core.task.optimizeTopDown
import ru.ifmo.fbsat.core.task.single.basic.basic
import ru.ifmo.fbsat.core.task.single.basic.basicMin
import ru.ifmo.fbsat.core.task.single.basic.basicMinC
import ru.ifmo.fbsat.core.task.single.basic.declareBasic
import ru.ifmo.fbsat.core.utils.checkMapping
import ru.ifmo.fbsat.core.utils.log

fun Inferrer.inferExtended(): Automaton? {
    val vars = solver.extendedVars
    val rawAssignment = solver.solve() ?: return null
    val assignment = ExtendedAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    with(vars) {
        check(
            automaton.checkMapping(
                scenarios = scenarioTree.scenarios,
                mapping = assignment.mapping
            )
        ) { "Positive mapping mismatch" }
    }

    return automaton
}

fun Inferrer.optimizeN(start: Int? = null, end: Int = 0): Automaton? {
    log.info("Optimizing N...")
    val vars = solver.extendedVars
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { N ->
            vars.cardinality.updateUpperBoundLessThanOrEqual(N)
            inferExtended()
        },
        next = { N ->
            vars.cardinality.updateUpperBoundLessThan(N)
            inferExtended()
        },
        query = { it.totalGuardsSize }
    )
}

fun Inferrer.extended(
    scenarioTree: ScenarioTree,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    isEncodeReverseImplication: Boolean = true
): Automaton? {
    reset()
    solver.declareBasic(
        scenarioTree = scenarioTree,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions,
        maxTransitions = maxTransitions,
        isEncodeReverseImplication = isEncodeReverseImplication
    )
    solver.declareExtended(
        maxGuardSize = maxGuardSize,
        maxTotalGuardsSize = maxTotalGuardsSize
    )
    return inferExtended()
}

fun Inferrer.extendedMin(
    scenarioTree: ScenarioTree,
    numberOfStates: Int? = null,
    maxGuardSize: Int // P
): Automaton? {
    basicMinC(scenarioTree, start = numberOfStates ?: 1)
    solver.declareExtended(maxGuardSize = maxGuardSize)
    return optimizeN()
}

@Suppress("LocalVariableName")
fun Inferrer.extendedMinUB(
    scenarioTree: ScenarioTree,
    numberOfStates: Int? = null,
    start: Int = 1, // P_start
    end: Int = 20, // P_end
    maxPlateauWidth: Int? = null // w, =Inf if null
): Automaton? {
    require(start >= 1)
    require(end >= 1)
    require(start <= end)

    val basicAutomaton =
        (if (numberOfStates != null) basic(scenarioTree, numberOfStates)
        else basicMin(scenarioTree)) ?: return null
    val C = basicAutomaton.numberOfStates
    val Tmin = basicAutomaton.numberOfTransitions
    var best: Automaton? = null
    var Plow: Int? = null
    var N: Int? = null // <=

    log.info("Tmin = $Tmin")

    for (P in start..end) {
        log.info("Trying P = $P, N = $N")

        if (best != null && P > (best.totalGuardsSize - Tmin)) {
            log.warn("Reached upper bound: P = $P, Plow = $Plow, Nbest = ${best.totalGuardsSize}, Tmin = $Tmin")
            break
        }
        if (Plow != null && maxPlateauWidth != null && (P - Plow) > maxPlateauWidth) {
            log.warn("Reached maximum plateau width: P = $P, Plow = $Plow, w = $maxPlateauWidth")
            break
        }

        reset()
        solver.declareBasic(scenarioTree, numberOfStates = C)
        solver.declareExtended(maxGuardSize = P)
        val automaton = optimizeN(start = N)
        if (automaton != null) {
            best = automaton
            Plow = P
            N = automaton.totalGuardsSize - 1
        }
    }

    return best
}
