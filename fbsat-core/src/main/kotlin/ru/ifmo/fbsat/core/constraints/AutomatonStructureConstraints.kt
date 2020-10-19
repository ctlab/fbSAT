@file:Suppress("LocalVariableName")

package ru.ifmo.fbsat.core.constraints

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.SolverContext
import ru.ifmo.fbsat.core.solver.atMostOne
import ru.ifmo.fbsat.core.solver.iff
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.implyAnd
import ru.ifmo.fbsat.core.solver.switchContext
import ru.ifmo.fbsat.core.utils.EpsilonOutputEvents
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.StartStateAlgorithms
import ru.ifmo.fbsat.core.utils.exhaustive

fun Solver.declareAutomatonStructureConstraints() {
    comment("Automaton structure constraints")

    comment("Automaton structure constraints: inputless")
    declareAutomatonStructureConstraintsInputless()

    val U: Int by context
    comment("Automaton structure constraints: for inputs (${1..U})")
    declareAutomatonStructureConstraintsForInputs(Us = 1..U)
}

fun Solver.declareNegativeAutomatonStructureConstraints(Us: Iterable<Int>) {
    comment("Negative automaton structure constraints")
    val negativeContext: SolverContext by context

    switchContext(negativeContext) {
        // Note: no inputless constraints

        comment("Negative automaton structure constraints: for inputs ($Us)")
        declareAutomatonStructureConstraintsForInputs(Us = Us)
    }
}

fun Solver.declareParallelModularAutomatonStructureConstraints() {
    // comment("Parallel modular automaton structure constraints")
    // val M: Int by context
    // val Z: Int by context
    // val modularContext: MultiArray<SolverContext> by context
    //
    //     for (m in 1..M) {
    //         comment("Parallel modular automaton structure constraints: for module m = $m")
    //         declareAutomatonStructureConstraints(context = modularContext[m])
    //     }
    //
    //     comment("Additional parallel modular structure constraints")
    //
    //     comment("EO")
    //     for (z in 1..Z)
    //         exactlyOne {
    //             for (m in 1..M)
    //                 yield(moduleControllingOutputVariable[z] eq m)
    //         }
    //     comment("ALO")
    //     for (m in 1..M)
    //         atLeastOne {
    //             for (z in 1..Z)
    //                 yield(moduleControllingOutputVariable[z] eq m)
    //         }
    //
    //     comment("Constraint free variables")
    //     for (m in 1..M) with(modularBasicVariables[m]) {
    //         for (z in 1..Z)
    //             for (c in 2..C) {
    //                 imply(moduleControllingOutputVariable[z] neq m, stateAlgorithmTop[c, z])
    //                 imply(moduleControllingOutputVariable[z] neq m, -stateAlgorithmBot[c, z])
    //             }
    //     }
}

fun Solver.declareConsecutiveModularAutomatonStructureConstraints() {
    check(Globals.EPSILON_OUTPUT_EVENTS == EpsilonOutputEvents.NONE)
    check(Globals.START_STATE_ALGORITHMS == StartStateAlgorithms.ZERONOTHING || Globals.START_STATE_ALGORITHMS == StartStateAlgorithms.ZERO)

    comment("Consecutive modular automaton structure constraints")
    val M: Int by context
    val modularContext: MultiArray<SolverContext> by context

    for (m in 1..M) switchContext(modularContext[m]) {
        comment("Consecutive modular automaton structure constraints for module m = $m: inputless")
        declareAutomatonStructureConstraintsInputless()
        // declareAutomatonStructureConstraintsInputless(
        //     context = modularContext[m]
        //     // Note: O = E, it is not a typo!
        //     // C = C, K = K, E = E, O = E, Z = Z,
        //     // stateOutputEvent = stateOutputEvent,
        //     // stateAlgorithmTop = stateAlgorithmTop,
        //     // stateAlgorithmBot = stateAlgorithmBot,
        //     // transitionDestination = transitionDestination,
        //     // transitionInputEvent = transitionInputEvent
        // )

        val U: Int by context
        comment("Consecutive modular automaton structure constraints for module m = $m: for inputs (${1..U})")
        declareAutomatonStructureConstraintsForInputs(Us = 1..U)

        /* Additional constraints */
        // TODO: Additional consecutive parallel constraints
    }
}

fun Solver.declareDistributedAutomatonStructureConstraints() {
    comment("Distributed automaton structure constraints")
    val M: Int by context
    val stateUsed: BoolVarArray by context
    val modularContext: MultiArray<SolverContext> by context

    for (m in 1..M) switchContext(modularContext[m]) {
        comment("Distributed automaton structure constraints: for module m = $m")
        declareAutomatonStructureConstraints()

        comment("Distributed automaton state usage constraints: for module m = $m")
        val C: Int by context
        val K: Int by context
        val transitionDestination: IntVarArray by context
        // val stateUsed: IntVarArray by context

        comment("Start state is always used")
        clause(stateUsed[1])

        comment("State non-usage propagation")
        for (c in 2 until C)
            imply(
                -stateUsed[c],
                -stateUsed[c + 1]
            )

        comment("Unused states don't have outgoing transitions")
        for (c in 2..C)
            implyAnd(-stateUsed[c], sequence {
                for (k in 1..K)
                    yield(transitionDestination[c, k] eq 0)
            })

        comment("(only) Unused states don't have incoming transitions")
        for (c in 2..C)
            iffAnd(-stateUsed[c], sequence {
                for (c2 in 1..C)
                    for (k in 1..K)
                        yield(transitionDestination[c2, k] neq c)
            })

        // TODO: constraints about stateOutputEvent
        // TODO: constraints about stateAlgorithm
    }
}

private fun Solver.declareAutomatonStructureConstraintsInputless() {
    val C: Int by context
    val K: Int by context
    val Z: Int by context
    val stateOutputEvent: IntVarArray by context
    val stateAlgorithmTop: BoolVarArray by context
    val stateAlgorithmBot: BoolVarArray by context
    val transitionDestination: IntVarArray by context
    val transitionInputEvent: IntVarArray by context

    when (Globals.EPSILON_OUTPUT_EVENTS) {
        EpsilonOutputEvents.START -> {
            comment("Start state produces epsilon event")
            clause(stateOutputEvent[1] eq 0)
        }
        EpsilonOutputEvents.ONLYSTART -> {
            comment("Only start state produces epsilon event")
            clause(stateOutputEvent[1] eq 0)
            for (c in 2..C)
                clause(stateOutputEvent[c] neq 0)
        }
        EpsilonOutputEvents.NONE -> {
            comment("No state can produce epsilon event")
            for (c in 1..C)
                clause(stateOutputEvent[c] neq 0)
        }
    }.exhaustive

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
        StartStateAlgorithms.INIT -> {
            comment("Start state algorithms are the same as init state algorithms")
            for (z in 1..Z) {
                val initVal = Globals.INITIAL_OUTPUT_VALUES[z - 1]
                val botLiteral = stateAlgorithmBot[1, z]
                val topLiteral = stateAlgorithmTop[1, z]
                clause(if (initVal) botLiteral else -botLiteral)
                clause(if (initVal) topLiteral else -topLiteral)
            }
        }
        StartStateAlgorithms.INITNOTHING -> {
            comment("Start state does not change initial values")
            for (z in 1..Z) {
                val initVal = Globals.INITIAL_OUTPUT_VALUES[z - 1]
                if (initVal)
                    clause(stateAlgorithmTop[1, z])
                else
                    clause(-stateAlgorithmBot[1, z])
            }
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

private fun Solver.declareAutomatonStructureConstraintsForInputs(Us: Iterable<Int>) {
    val C: Int by context
    val K: Int by context
    val E: Int by context
    val transitionDestination: IntVarArray by context
    val transitionInputEvent: IntVarArray by context
    val transitionTruthTable: BoolVarArray by context
    val transitionFiring: BoolVarArray by context
    val firstFired: IntVarArray by context
    val notFired: BoolVarArray by context
    val actualTransitionFunction: IntVarArray by context

    // TODO: Remove
    // comment("Guards on not-null transitions are not False")
    // // (transitionDestination[c,k] != 0) => ALO_{u}( transitionTruthTable[c,k,u] )
    // @Suppress("ReplaceCollectionCountWithSize")
    // if (Us.count() > 0)
    //     for (c in 1..C)
    //         for (k in 1..K)
    //             implyOr(transitionDestination[c, k] neq 0, sequence {
    //                 for (u in Us)
    //                     yield(transitionTruthTable[c, k, u])
    //             })

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
