@file:Suppress("LocalVariableName")

package ru.ifmo.fbsat.core.constraints

import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.eq
import com.github.lipen.satlib.core.neq
import com.github.lipen.satlib.op.atLeastOne
import com.github.lipen.satlib.op.atMostOne
import com.github.lipen.satlib.op.exactlyOne
import com.github.lipen.satlib.op.iff
import com.github.lipen.satlib.op.iffAnd
import com.github.lipen.satlib.op.imply
import com.github.lipen.satlib.op.implyAnd
import com.github.lipen.satlib.op.implyOr
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.solver.autoneg
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.forEachModularContext
import ru.ifmo.fbsat.core.solver.imply2
import ru.ifmo.fbsat.core.utils.EpsilonOutputEvents
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.StartStateAlgorithms
import ru.ifmo.fbsat.core.utils.exhaustive

private val logger = MyLogger {}

fun Solver.declareAutomatonStructureConstraints() {
    comment("Automaton structure constraints")

    comment("Automaton structure constraints: inputless")
    declareAutomatonStructureConstraintsInputless()

    val U: Int = context["U"]
    comment("Automaton structure constraints: for inputs (${1..U})")
    declareAutomatonStructureConstraintsForInputs(Us = 1..U, isPositive = true)
}

fun Solver.declareNegativeAutomatonStructureConstraints(Us: Iterable<Int>) {
    comment("Negative automaton structure constraints")

    // Note: no inputless constraints

    comment("Negative automaton structure constraints: for inputs ($Us)")
    declareAutomatonStructureConstraintsForInputs(Us = Us, isPositive = false)
}

fun Solver.declareParallelModularAutomatonStructureConstraints() {
    comment("Parallel modular automaton structure constraints")
    val M: Int = context["M"]
    val Z: Int = context["Z"]
    val moduleControllingOutputVariable: IntVarArray = context["moduleControllingOutputVariable"]

    forEachModularContext { m ->
        comment("Parallel modular automaton structure constraints: for module m = $m")
        declareAutomatonStructureConstraints()
    }

    comment("Additional parallel modular structure constraints")

    comment("EO")
    for (z in 1..Z)
        exactlyOne {
            for (m in 1..M)
                yield(moduleControllingOutputVariable[z] eq m)
        }

    comment("ALO")
    for (m in 1..M)
        atLeastOne {
            for (z in 1..Z)
                yield(moduleControllingOutputVariable[z] eq m)
        }

    comment("Constraint free variables")
    forEachModularContext { m ->
        val C: Int = context["C"]
        val stateAlgorithmBot: BoolVarArray = context["stateAlgorithmBot"]
        val stateAlgorithmTop: BoolVarArray = context["stateAlgorithmTop"]
        for (z in 1..Z)
            for (c in 2..C) {
                imply(moduleControllingOutputVariable[z] neq m, stateAlgorithmTop[c, z])
                imply(moduleControllingOutputVariable[z] neq m, -stateAlgorithmBot[c, z])
            }
    }
}

fun Solver.declareConsecutiveModularAutomatonStructureConstraints() {
    check(Globals.EPSILON_OUTPUT_EVENTS == EpsilonOutputEvents.NONE)
    check(Globals.START_STATE_ALGORITHMS == StartStateAlgorithms.ZERONOTHING || Globals.START_STATE_ALGORITHMS == StartStateAlgorithms.ZERO)

    comment("Consecutive modular automaton structure constraints")
    forEachModularContext { m ->
        comment("Consecutive modular automaton structure constraints for module m = $m: inputless")
        declareAutomatonStructureConstraintsInputless()

        val U: Int = context["U"]
        comment("Consecutive modular automaton structure constraints for module m = $m: for inputs (${1..U})")
        declareAutomatonStructureConstraintsForInputs(Us = 1..U, isPositive = true)

        /* Additional constraints */
        // TODO: Additional consecutive parallel constraints
    }
}

@Suppress("LocalVariableName")
fun Solver.declareArbitraryModularAutomatonStructureConstraints() {
    check(Globals.EPSILON_OUTPUT_EVENTS == EpsilonOutputEvents.NONE) {
        "Globals.EPSILON_OUTPUT_EVENTS must be NONE"
    }
    check(Globals.START_STATE_ALGORITHMS == StartStateAlgorithms.ZERONOTHING || Globals.START_STATE_ALGORITHMS == StartStateAlgorithms.ZERO) {
        "Globals.START_STATE_ALGORITHMS must be either ZERONOTHING or ZERO"
    }

    comment("Arbitrary modular automaton structure constraints")
    forEachModularContext { m ->
        comment("Arbitrary modular automaton structure constraints for module m = $m: inputless")
        declareAutomatonStructureConstraintsInputless()

        val U: Int = context["U"]
        comment("Arbitrary modular automaton structure constraints for module m = $m: for inputs (${1..U})")
        declareAutomatonStructureConstraintsForInputs(Us = 1..U, isPositive = true)
    }
}

fun Solver.declareDistributedAutomatonStructureConstraints() {
    comment("Distributed automaton structure constraints")
    forEachModularContext { m ->
        comment("Distributed automaton structure constraints: for module m = $m")
        declareAutomatonStructureConstraints()

        comment("Distributed automaton state usage constraints: for module m = $m")
        val C: Int = context["C"]
        val K: Int = context["K"]
        val transitionDestination: IntVarArray = context["transitionDestination"]
        val stateUsed: BoolVarArray = context["stateUsed"]

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
            implyAnd(-stateUsed[c]) {
                for (k in 1..K)
                    yield(transitionDestination[c, k] eq 0)
            }

        comment("(only) Unused states don't have incoming transitions")
        for (c in 2..C)
            iffAnd(-stateUsed[c]) {
                for (c2 in 1..C)
                    for (k in 1..K)
                        yield(transitionDestination[c2, k] neq c)
            }

        // TODO: constraints about stateOutputEvent
        // TODO: constraints about stateAlgorithm
    }
}

// TODO: change `internal` back to `private`
internal fun Solver.declareAutomatonStructureConstraintsInputless() {
    val C: Int = context["C"]
    val K: Int = context["K"]
    val Z: Int = context["Z"]
    val stateOutputEvent: IntVarArray = context["stateOutputEvent"]
    val stateAlgorithmTop: BoolVarArray = context["stateAlgorithmTop"]
    val stateAlgorithmBot: BoolVarArray = context["stateAlgorithmBot"]
    val transitionDestination: IntVarArray = context["transitionDestination"]
    val transitionInputEvent: IntVarArray = context["transitionInputEvent"]
    val initialOutputValues: OutputValues = context["initialOutputValues"]

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
                val initVal = initialOutputValues[z - 1]
                val botLiteral = stateAlgorithmBot[1, z]
                val topLiteral = stateAlgorithmTop[1, z]
                clause(if (initVal) botLiteral else -botLiteral)
                clause(if (initVal) topLiteral else -topLiteral)
            }
        }
        StartStateAlgorithms.INITNOTHING -> {
            comment("Start state does not change initial values")
            for (z in 1..Z) {
                val initVal = initialOutputValues[z - 1]
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

// TODO: change `internal` back to `private`
internal fun Solver.declareAutomatonStructureConstraintsForInputs(
    Us: Iterable<Int>,
    isPositive: Boolean,
) {
    val C: Int = context["C"]
    val K: Int = context["K"]
    val E: Int = context["E"]
    val actualTransitionFunction: IntVarArray = context.autoneg("actualTransitionFunction", isPositive)
    val transitionDestination: IntVarArray = context["transitionDestination"]
    val transitionInputEvent: IntVarArray = context["transitionInputEvent"]
    val transitionTruthTable: BoolVarArray = context.autoneg("transitionTruthTable", isPositive)
    val transitionFiring: BoolVarArray = context.autoneg("transitionFiring", isPositive)
    val firstFired: IntVarArray = context.autoneg("firstFired", isPositive)
    val notFired: BoolVarArray = context.autoneg("notFired", isPositive)

    // TODO: Remove
    // comment("Guards on not-null transitions are not False")
    // // (transitionDestination[c,k] != 0) => ALO_{u}( transitionTruthTable[c,k,u] )
    // @Suppress("ReplaceCollectionCountWithSize")
    // if (Us.count() > 0)
    //     for (c in 1..C)
    //         for (k in 1..K)
    //             implyOr(transitionDestination[c, k] neq 0) {
    //                 for (u in Us)
    //                     yield(transitionTruthTable[c, k, u])
    //             }

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
    // notFired[k] <=> ~transitionFiring[k] & notFired[k-1]
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

    if (!Globals.IS_ENCODE_FF_NF_VARDECL) {
        comment("Shortcut: firstFired[0] <=> notFired[K]")
        // firstFired[0] <=> notFired[K}
        for (c in 1..C)
            for (e in 1..E)
                for (u in Us)
                    iff(
                        firstFired[c, e, u] eq 0,
                        notFired[c, K, e, u]
                    )
    }

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
    //  OR_k ( (transitionDestination[q,k] = q') & (firstFired[q,e,u] = k) )
    // for (i in 1..C)
    //     for (e in 1..E)
    //         for (u in Us)
    //             for (j in 1..C)
    //                 iffOr(actualTransitionFunction[i, e, u] eq j) {
    //                     for (k in 1..K) {
    //                         // aux <=> (transitionDestination[q,k] = q') & (firstFired[q,e,u] = k)
    //                         val aux = newLiteral()
    //                         iffAnd(
    //                             aux,
    //                             transitionDestination[i, k] eq j,
    //                             firstFired[i, e, u] eq k
    //                         )
    //                         yield(aux)
    //                     }
    //                 }
    // t & ff => atf
    for (i in 1..C)
        for (e in 1..E)
            for (u in Us)
                for (j in 1..C)
                    for (k in 1..K)
                        imply2(
                            transitionDestination[i, k] eq j,
                            firstFired[i, e, u] eq k,
                            actualTransitionFunction[i, e, u] eq j
                        )
    // atf => OR_k (t & ff)
    for (i in 1..C)
        for (e in 1..E)
            for (u in Us)
                for (j in 1..C)
                    implyOr(actualTransitionFunction[i, e, u] eq j) {
                        for (k in 1..K) {
                            // aux <=> (transitionDestination[q,k] = q') & (firstFired[q,e,u] = k)
                            val aux = newLiteral()
                            iffAnd(
                                aux,
                                transitionDestination[i, k] eq j,
                                firstFired[i, e, u] eq k
                            )
                            yield(aux)
                        }
                    }

    if (Globals.IS_ENCODE_ATF_0) {
        if (!Globals.IS_ENCODE_FF_0_VARDECL) {
            // (actualTransitionFunction[q,e,u] = 0) <=> (firstFired[q,e,u] = 0)
            for (c in 1..C)
                for (e in 1..E)
                    for (u in Us)
                        iff(
                            actualTransitionFunction[c, e, u] eq 0,
                            firstFired[c, e, u] eq 0
                        )
        } else {
            logger.warn("IS_ENCODE_ATF_0 and IS_ENCODE_FF_0_VARDECL are both True. Constraint is NOT declared")
        }
    }

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
