package ru.ifmo.fbsat.core.constraints

import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.iff
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.ArbitraryModularBasicVariables
import ru.ifmo.fbsat.core.utils.EpsilonOutputEvents
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.StartStateAlgorithms
import ru.ifmo.fbsat.core.utils.exhaustive

fun Solver.declareArbitraryModularAutomatonStructureConstraints(
    arbitraryModularBasicVariables: ArbitraryModularBasicVariables
) {
    check(Globals.EPSILON_OUTPUT_EVENTS == EpsilonOutputEvents.NONE)
    check(Globals.START_STATE_ALGORITHMS == StartStateAlgorithms.ZERONOTHING || Globals.START_STATE_ALGORITHMS == StartStateAlgorithms.ZERO)

    comment("Arbitrary modular automaton structure constraints")
    with(arbitraryModularBasicVariables) {
        for (m in 1..M) {
            comment("Arbitrary modular automaton structure constraints: for module m = $m")
            declareModuleAutomatonStructureConstraints(
                C = C, K = K,
                Z = Z, U = U,
                actualTransitionFunction = modularActualTransitionFunction[m],
                transitionDestination = modularTransitionDestination[m],
                transitionFiring = modularTransitionFiring[m],
                firstFired = modularFirstFired[m],
                notFired = modularNotFired[m],
                stateAlgorithmTop = modularStateAlgorithmTop[m],
                stateAlgorithmBot = modularStateAlgorithmBot[m]
            )
        }

        /* Additional constraints */

        // TODO: Additional arbitrary modular constraints
    }
}

private fun Solver.declareModuleAutomatonStructureConstraints(
    C: Int,
    K: Int,
    Z: Int,
    U: Int,
    actualTransitionFunction: IntVarArray,
    transitionDestination: IntVarArray,
    transitionFiring: BoolVarArray,
    firstFired: IntVarArray,
    notFired: BoolVarArray,
    stateAlgorithmTop: BoolVarArray,
    stateAlgorithmBot: BoolVarArray
) {
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
    }.exhaustive

    comment("Null-transitions are last")
    // (transitionDestination[k] = 0) => (transitionDestination[k+1] = 0)
    for (c in 1..C)
        for (k in 1 until K)
            imply(
                transitionDestination[c, k] eq 0,
                transitionDestination[c, k + 1] eq 0
            )

    comment("First fired definition")
    // (firstFired = k) <=> transitionFiring[k] & notFired[k-1]
    for (c in 1..C)
        for (u in 1..U) {
            iff(
                firstFired[c, u] eq 1,
                transitionFiring[c, 1, u]
            )
            for (k in 2..K)
                iffAnd(
                    firstFired[c, u] eq k,
                    transitionFiring[c, k, u],
                    notFired[c, k - 1, u]
                )
        }

    comment("Not fired definition")
    // not_fired[k] <=> ~transitionFiring[k] & notFired[k-1]
    for (c in 1..C)
        for (u in 1..U) {
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
        for (u in 1..U)
            iff(
                firstFired[c, u] eq 0,
                notFired[c, K, u]
            )

    comment("Propagation of not-notFired (maybe redundant)")
    // ~notFired[k] => ~notFired[k+1]
    for (c in 1..C)
        for (k in 1 until K)
            for (u in 1..U)
                imply(
                    -notFired[c, k, u],
                    -notFired[c, k + 1, u]
                )

    comment("Actual transition function definition")
    // (actualTransitionFunction[q,u] = q') <=>
    //  OR_k ( (transitionDestination[q,k] = q') & (firstFired[q,u] = k) )
    for (i in 1..C)
        for (j in 1..C)
            for (u in 1..U)
                iffOr(actualTransitionFunction[i, u] eq j, sequence {
                    for (k in 1..K) {
                        // aux <=> (transitionDestination[q,k] = q') & (transitionInputEvent[q,k] = e) & (firstFired[q,u] = k)
                        val aux = newLiteral()
                        iffAnd(
                            aux,
                            transitionDestination[i, k] eq j,
                            firstFired[i, u] eq k
                        )
                        yield(aux)
                    }
                })
}
