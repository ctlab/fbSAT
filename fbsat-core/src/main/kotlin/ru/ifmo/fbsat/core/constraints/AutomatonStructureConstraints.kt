package ru.ifmo.fbsat.core.constraints

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.atLeastOne
import ru.ifmo.fbsat.core.solver.exactlyOne
import ru.ifmo.fbsat.core.solver.iff
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.task.modular.basic.parallel.ParallelModularBasicVariables
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.complete.CompleteVariables
import ru.ifmo.fbsat.core.utils.EpsilonOutputEvents
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.StartStateAlgorithms

fun Solver.declareAutomatonStructureConstraints(basicVariables: BasicVariables) {
    comment("Automaton structure constraints")
    with(basicVariables) {
        comment("Automaton structure constraints: inputless")
        declareAutomatonStructureConstraintsInputless(
            C = C, K = K, E = E, O = O, Z = Z,
            stateOutputEvent = stateOutputEvent,
            stateAlgorithmTop = stateAlgorithmTop,
            stateAlgorithmBot = stateAlgorithmBot,
            transitionDestination = transitionDestination,
            transitionInputEvent = transitionInputEvent
        )

        for (u in 1..U) {
            comment("Automaton structure constraints: for input u = $u")
            declareAutomatonStructureConstraintsForInput(
                u = u,
                C = C, K = K, E = E,
                transitionDestination = transitionDestination,
                transitionInputEvent = transitionInputEvent,
                transitionFiring = transitionFiring,
                firstFired = firstFired,
                notFired = notFired,
                actualTransitionFunction = actualTransitionFunction
            )
        }
    }
}

fun Solver.declareNegativeAutomatonStructureConstraints(
    completeVars: CompleteVariables,
    Us: Iterable<Int>
) {
    comment("Negative automaton structure constraints")
    with(completeVars) {
        // Note: no inputless constraints

        for (u in Us) {
            // Note: be very careful with positive/negative variables!
            comment("Negative automaton structure constraints: for input u = $u")
            declareAutomatonStructureConstraintsForInput(
                u = u,
                C = C, K = K, E = E,
                transitionDestination = transitionDestination,
                transitionInputEvent = transitionInputEvent,
                transitionFiring = negTransitionFiring,
                firstFired = negFirstFired,
                notFired = negNotFired,
                actualTransitionFunction = negActualTransitionFunction
            )
        }
    }
}

fun Solver.declareParallelModularAutomatonStructureConstraints(
    parallelModularBasicVariables: ParallelModularBasicVariables
) {
    comment("Parallel modular automaton structure constraints")
    with(parallelModularBasicVariables) {
        for (m in 1..M) {
            comment("Automaton structure constraints: for module m = $m")
            declareAutomatonStructureConstraints(modularBasicVariables[m])
        }

        comment("Additional parallel modular structure constraints")

        // EO
        for (z in 1..Z)
            exactlyOne {
                for (m in 1..M)
                    yield(moduleControllingOutputVariable[z, m])
            }
        // ALO
        for (m in 1..M)
            atLeastOne {
                for (z in 1..Z)
                    yield(moduleControllingOutputVariable[z, m])
            }

        comment("Constraint free variables")
        for (m in 1..M)
            for (z in 1..Z)
                for (c in 2..C) {
                    imply(-moduleControllingOutputVariable[z, m], modularStateAlgorithmTop[m][c, z])
                    imply(-moduleControllingOutputVariable[z, m], -modularStateAlgorithmBot[m][c, z])
                }
    }
}

private fun Solver.declareAutomatonStructureConstraintsInputless(
    C: Int,
    K: Int,
    E: Int,
    O: Int,
    Z: Int,
    stateOutputEvent: IntMultiArray,
    stateAlgorithmTop: IntMultiArray,
    stateAlgorithmBot: IntMultiArray,
    transitionDestination: IntMultiArray,
    transitionInputEvent: IntMultiArray
) {
    when (Globals.EPSILON_OUTPUT_EVENTS) {
        EpsilonOutputEvents.START -> {
            comment("Start state produces epsilon event")
            clause(stateOutputEvent[1, O + 1])
        }
        EpsilonOutputEvents.ONLYSTART -> {
            comment("Only start state produces epsilon event")
            clause(stateOutputEvent[1, O + 1])
            for (c in 2..C)
                clause(-stateOutputEvent[c, O + 1])
        }
        EpsilonOutputEvents.NONE -> {
            comment("No state can produce epsilon event")
            for (c in 1..C)
                clause(-stateOutputEvent[c, O + 1])
        }
    }

    when (Globals.START_STATE_ALGORITHMS) {
        StartStateAlgorithms.NOTHING -> {
            comment("Start state does nothing")
            for (z in 1..Z) {
                clause(stateAlgorithmTop[1, z])
                clause(-stateAlgorithmBot[1, z])
            }
        }
        StartStateAlgorithms.ZERO -> {
            comment("Start state produces zeros")
            for (z in 1..Z) {
                clause(-stateAlgorithmTop[1, z])
                clause(-stateAlgorithmBot[1, z])
            }
        }
        StartStateAlgorithms.ZERONOTHING -> {
            comment("Start state does not change zeros")
            for (z in 1..Z)
                clause(-stateAlgorithmBot[1, z])
        }
        StartStateAlgorithms.ANY -> {
            comment("Start state algorithms may be arbitrary")
        }
    }

    comment("Null-transitions are last")
    // (transitionDestination[k] = 0) => (transitionDestination[k+1] = 0)
    for (c in 1..C)
        for (k in 1 until K)
            imply(
                transitionDestination[c, k, C + 1],
                transitionDestination[c, k + 1, C + 1]
            )

    comment("Only null-transitions have no input event")
    // (transitionDestination[k] = 0) <=> (transitionInputEvent[k] = 0)
    for (c in 1..C)
        for (k in 1..K)
            iff(
                transitionDestination[c, k, C + 1],
                transitionInputEvent[c, k, E + 1]
            )
}

private fun Solver.declareAutomatonStructureConstraintsForInput(
    u: Int,
    C: Int,
    K: Int,
    E: Int,
    transitionDestination: IntMultiArray,
    transitionInputEvent: IntMultiArray,
    transitionFiring: IntMultiArray,
    firstFired: IntMultiArray,
    notFired: IntMultiArray,
    actualTransitionFunction: IntMultiArray
) {
    comment("First fired definition")
    // (firstFired = k) <=> transitionFiring[k] & notFired[k-1]
    for (c in 1..C) {
        iff(
            firstFired[c, u, 1],
            transitionFiring[c, 1, u]
        )
        for (k in 2..K)
            iffAnd(
                firstFired[c, u, k],
                transitionFiring[c, k, u],
                notFired[c, k - 1, u]
            )
    }

    comment("Not fired definition")
    // not_fired[k] <=> ~transitionFiring[k] & notFired[k-1]
    for (c in 1..C) {
        iff(
            notFired[c, 1, u],
            -transitionFiring[c, 1, u]
        )
        for (k in 2..K)
            iffAnd(
                notFired[c, k, u],
                -transitionFiring[c, k, u],
                notFired[c, k - 1, u]
            )
    }

    comment("Shortcut: firstFired[0] <=> notFired[K]")
    // firstFired[0] <=> notFired[K}
    for (c in 1..C)
        iff(
            firstFired[c, u, K + 1],
            notFired[c, K, u]
        )

    comment("Propagation of not-notFired (maybe redundant)")
    // ~notFired[k] => ~notFired[k+1]
    for (c in 1..C)
        for (k in 1 until K)
            imply(
                -notFired[c, k, u],
                -notFired[c, k + 1, u]
            )

    comment("Actual transition function definition for u = $u")
    // (actualTransitionFunction[q,e,u] = q') <=>
    //  OR_k ( (transitionDestination[q,k] = q') & (transitionInputEvent[q,k] = e) & (firstFired[q,u] = k) )
    for (i in 1..C)
        for (e in 1..E)
            for (j in 1..C)
                iffOr(actualTransitionFunction[i, e, u, j], sequence {
                    for (k in 1..K) {
                        // aux <=> (transitionDestination[q,k] = q') & (transitionInputEvent[q,k] = e) & (firstFired[q,u] = k)
                        val aux = newVariable()
                        iffAnd(
                            aux,
                            transitionDestination[i, k, j],
                            transitionInputEvent[i, k, e],
                            firstFired[i, u, k]
                        )
                        yield(aux)
                    }
                })
}
