package ru.ifmo.fbsat.core.constraints2

import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.atMostOne
import ru.ifmo.fbsat.core.solver.iff
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.implyOr
import ru.ifmo.fbsat.core.task.single.basic2.BasicVariables2
import ru.ifmo.fbsat.core.utils.EpsilonOutputEvents
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.StartStateAlgorithms
import ru.ifmo.fbsat.core.utils.exhaustive

fun Solver.declareAutomatonStructureConstraints2(basicVariables: BasicVariables2) {
    comment("Automaton structure constraints")
    with(basicVariables) {
        comment("Automaton structure constraints: inputless")
        declareAutomatonStructureConstraintsInputless2(
            C = C, K = K, E = E, O = O, Z = Z,
            stateOutputEvent = stateOutputEvent,
            stateAlgorithmTop = stateAlgorithmTop,
            stateAlgorithmBot = stateAlgorithmBot,
            transitionDestination = transitionDestination,
            transitionInputEvent = transitionInputEvent
        )

        comment("Automaton structure constraints: for inputs")
        declareAutomatonStructureConstraintsForInputs2(
            C = C, K = K, E = E, Us = 1..U,
            transitionDestination = transitionDestination,
            transitionInputEvent = transitionInputEvent,
            transitionTruthTable = transitionTruthTable,
            transitionFiring = transitionFiring,
            firstFired = firstFired,
            notFired = notFired,
            actualTransitionFunction = actualTransitionFunction
        )
    }
}

private fun Solver.declareAutomatonStructureConstraintsInputless2(
    C: Int,
    K: Int,
    E: Int,
    O: Int,
    Z: Int,
    stateOutputEvent: IntVarArray,
    stateAlgorithmTop: BoolVarArray,
    stateAlgorithmBot: BoolVarArray,
    transitionDestination: IntVarArray,
    transitionInputEvent: IntVarArray
) {
    // when (Globals.EPSILON_OUTPUT_EVENTS) {
    //     EpsilonOutputEvents.START -> {
    //         comment("Start state produces epsilon event")
    //         clause(stateOutputEvent[1] eq 0)
    //     }
    //     EpsilonOutputEvents.ONLYSTART -> {
    //         comment("Only start state produces epsilon event")
    //         clause(stateOutputEvent[1] eq 0)
    //         for (c in 2..C)
    //             clause(stateOutputEvent[c] neq 0)
    //     }
    //     EpsilonOutputEvents.NONE -> {
    //         comment("No state can produce epsilon event")
    //         for (c in 1..C)
    //             clause(stateOutputEvent[c] neq 0)
    //     }
    // }.exhaustive

    // when (Globals.START_STATE_ALGORITHMS) {
    //     StartStateAlgorithms.NOTHING -> {
    //         comment("Start state does nothing")
    //         for (z in 1..Z) {
    //             clause(stateAlgorithmTop[1, z])
    //             clause(-stateAlgorithmBot[1, z])
    //         }
    //     }
    //     StartStateAlgorithms.ZERO -> {
    //         comment("Start state produces zeros")
    //         for (z in 1..Z) {
    //             clause(-stateAlgorithmTop[1, z])
    //             clause(-stateAlgorithmBot[1, z])
    //         }
    //     }
    //     StartStateAlgorithms.ZERONOTHING -> {
    //         comment("Start state does not change zeros")
    //         for (z in 1..Z)
    //             clause(-stateAlgorithmBot[1, z])
    //     }
    //     StartStateAlgorithms.ANY -> {
    //         comment("Start state algorithms may be arbitrary")
    //     }
    //     StartStateAlgorithms.INIT -> {
    //         comment("Start state algorithms are the same as init state algorithms")
    //         for (z in 1..Z) {
    //             val initVal = Globals.INITIAL_OUTPUT_VALUES[z - 1]
    //             val botLiteral = stateAlgorithmBot[1, z]
    //             val topLiteral = stateAlgorithmTop[1, z]
    //             clause(if (initVal) botLiteral else -botLiteral)
    //             clause(if (initVal) topLiteral else -topLiteral)
    //         }
    //     }
    //     StartStateAlgorithms.INITNOTHING -> {
    //         comment("Start state does not change initial values")
    //         for (z in 1..Z) {
    //             val initVal = Globals.INITIAL_OUTPUT_VALUES[z - 1]
    //             if (initVal)
    //                 clause(stateAlgorithmTop[1, z])
    //             else
    //                 clause(-stateAlgorithmBot[1, z])
    //         }
    //     }
    // }.exhaustive

    comment("Null-transitions are last")
    // (transitionDestination[k] = 0) => (transitionDestination[k+1] = 0)
    for (c in 1..C)
        for (k in 1 until K)
            imply(
                transitionDestination[c, k] eq 0,
                transitionDestination[c, k + 1] eq 0
            )

    comment("Only null-transitions have no input event")
    // (transitionDestination[k] = 0) <=> (transitionInputEvent[k] = 0)
    for (c in 1..C)
        for (k in 1..K)
            iff(
                transitionDestination[c, k] eq 0,
                transitionInputEvent[c, k] eq 0
            )

    if (Globals.IS_FORBID_TRANSITIONS_TO_FIRST_STATE) {
        comment("Ad-hoc: no transitions to the first state")
        for (c in 1..C)
            for (k in 1..K)
                clause(transitionDestination[c, k] neq 1)
    }
}

private fun Solver.declareAutomatonStructureConstraintsForInputs2(
    C: Int,
    K: Int,
    E: Int,
    Us: Iterable<Int>,
    transitionDestination: IntVarArray,
    transitionInputEvent: IntVarArray,
    transitionTruthTable: BoolVarArray,
    transitionFiring: BoolVarArray,
    firstFired: IntVarArray,
    notFired: BoolVarArray,
    actualTransitionFunction: IntVarArray
) {
    comment("Guards on not-null transitions are not False")
    // (transitionDestination[c,k] != 0) => ALO_{u}( transitionTruthTable[c,k,u] )
    @Suppress("ReplaceCollectionCountWithSize")
    if (Us.count() > 0)
        for (c in 1..C)
            for (k in 1..K)
                implyOr(transitionDestination[c, k] neq 0, sequence {
                    for (u in Us)
                        yield(transitionTruthTable[c, k, u])
                })

    comment("Transition firing definition")
    // transitionFiring[c,k,e,u] <=> (transitionInputEvent[c,k] = e) & transitionTruthTable[c,k,u]
    for (c in 1..C)
        for (k in 1..K)
            for (e in 1..E)
                for (u in Us)
                    iffAnd(
                        transitionFiring[c, k, e, u],
                        transitionInputEvent[c, k] eq e,
                        transitionTruthTable[c, k, u]
                    )

    comment("First fired definition")
    // (firstFired = k) <=> transitionFiring[k] & notFired[k-1]
    for (c in 1..C)
        for (e in 1..E)
            for (u in Us) {
                iff(
                    firstFired[c, e, u] eq 1,
                    transitionFiring[c, 1, e, u]
                )
                for (k in 2..K)
                    iffAnd(
                        firstFired[c, e, u] eq k,
                        transitionFiring[c, k, e, u],
                        notFired[c, k - 1, e, u]
                    )
            }

    comment("Not fired definition")
    // not_fired[k] <=> ~transitionFiring[k] & notFired[k-1]
    for (c in 1..C)
        for (e in 1..E)
            for (u in Us) {
                iff(
                    notFired[c, 1, e, u],
                    -transitionFiring[c, 1, e, u]
                )
                for (k in 2..K)
                    iffAnd(
                        notFired[c, k, e, u],
                        -transitionFiring[c, k, e, u],
                        notFired[c, k - 1, e, u]
                    )
            }

    comment("Shortcut: firstFired[0] <=> notFired[K]")
    // firstFired[0] <=> notFired[K}
    for (c in 1..C)
        for (e in 1..E)
            for (u in Us)
                iff(
                    firstFired[c, e, u] eq 0,
                    notFired[c, K, e, u]
                )

    comment("Propagation of not-notFired (maybe redundant)")
    // ~notFired[k] => ~notFired[k+1]
    for (c in 1..C)
        for (k in 1 until K)
            for (e in 1..E)
                for (u in Us)
                    imply(
                        -notFired[c, k, e, u],
                        -notFired[c, k + 1, e, u]
                    )

    comment("Actual transition function definition")
    // (actualTransitionFunction[q,e,u] = q') <=>
    //  OR_k ( (transitionDestination[q,k] = q') & (transitionInputEvent[q,k] = e) & (firstFired[q,u] = k) )
    for (i in 1..C)
        for (e in 1..E)
            for (u in Us)
                for (j in 1..C)
                    iffOr(actualTransitionFunction[i, e, u] eq j, sequence {
                        for (k in 1..K) {
                            // aux <=> (transitionDestination[q,k] = q') & (transitionInputEvent[q,k] = e) & (firstFired[q,u] = k)
                            val aux = newLiteral()
                            iffAnd(
                                aux,
                                transitionDestination[i, k] eq j,
                                firstFired[i, e, u] eq k
                            )
                            yield(aux)
                        }
                    })

    if (Globals.IS_ENCODE_DISJUNCTIVE_TRANSITIONS) {
        comment("Transitions are disjunctive (without priority function)")
        for (c in 1..C)
            for (e in 1..E)
                for (u in Us)
                    atMostOne {
                        for (k in 1..K)
                            yield(transitionFiring[c, k, e, u])
                    }
    }
}
