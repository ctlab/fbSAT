@file:Suppress("LocalVariableName")

package ru.ifmo.fbsat.core.constraints

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.SolverContext
import ru.ifmo.fbsat.core.solver.iff
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newIntVarArray

fun Solver.declareAutomatonBfsConstraints(
    context: SolverContext = this.context
) {
    declareAutomatonBfsConstraintsImpl(context = context)
}

fun Solver.declareParallelModularAutomatonBfsConstraints() {
    // comment("Parallel modular automaton BFS constraints")
    // val modularContext: MultiArray<SolverContext> by context
    //
    //     for (m in 1..M) {
    //         comment("Automaton BFS constraints: for module m = $m")
    //         declareAutomatonBfsConstraints(context=modularContext[m])
    //     }
}

fun Solver.declareConsecutiveModularAutomatonBfsConstraints() {
    // comment("Consecutive modular automaton BFS constraints")
    // with(consecutiveModularBasicVariables) {
    //     for (m in 1..M) {
    //         comment("Automaton BFS constraints: for module m = $m")
    //         declareAutomatonBfsConstraints(modularBasicVariables[m])
    //     }
    // }
}

fun Solver.declareArbitraryModularAutomatonBfsConstraints() {
    // comment("Arbitrary modular automaton BFS constraints")
    // with(arbitraryModularBasicVariables) {
    //     for (m in 1..M) {
    //         comment("Automaton BFS constraints: for module m = $m")
    //         declareAutomatonBfsConstraintsImpl(
    //             C = C, K = K,
    //             transitionDestination = modularTransitionDestination[m]
    //         )
    //     }
    // }
}

fun Solver.declareDistributedAutomatonBfsConstraints() {
    comment("Distributed automaton BFS constraints")
    val modularContext: MultiArray<SolverContext> by context
    val M: Int by context
    // val stateUsed: BoolVarArray by context

    for (m in 1..M) {
        comment("Automaton BFS constraints: for module m = $m")
        // val C: Int by modularContext[m]
        // modularContext[m]["stateUsed"] = BoolVarArray.create(C) { (c) -> stateUsed[m, c] }
        declareAutomatonBfsConstraintsImpl_stateUsed(context = modularContext[m])
    }
}

private fun Solver.declareAutomatonBfsConstraintsImpl(
    context: SolverContext
) {
    comment("Automaton BFS constraints")
    val C: Int by context
    val K: Int by context
    val transitionDestination: IntVarArray by context
    val bfsTransitionAutomaton = newBoolVarArray(C, C)
    val bfsParentAutomaton = newIntVarArray(C) { (c) -> 1 until c }

    comment("bfs_t definition")
    // t[i, j] <=> OR_k( transition[i, k, j] )
    for (j in 1..C)
        for (i in 1..C)
            iffOr(bfsTransitionAutomaton[i, j], sequence {
                for (k in 1..K)
                    yield(transitionDestination[i, k] eq j)
            })

    comment("bfs_p definition")
    // (p[j] = i) <=> t[i, j] & AND_{r<i}( ~t[r, j] ) :: i<j
    for (j in 1..C)
        for (i in 1 until j)
            iffAnd(bfsParentAutomaton[j] eq i, sequence {
                yield(bfsTransitionAutomaton[i, j])
                for (r in 1 until i)
                    yield(-bfsTransitionAutomaton[r, j])
            })

    comment("BFS(p)")
    // (p[j] = i) => (p[j+1] != r) :: LB<=r<i<j<UB
    for (j in 3 until C)
        for (i in 2 until j)
            for (r in 1 until i)
                imply(
                    bfsParentAutomaton[j] eq i,
                    bfsParentAutomaton[j + 1] neq r
                )
}

private fun Solver.declareAutomatonBfsConstraintsImpl_stateUsed(
    context: SolverContext
) {
    comment("Automaton BFS constraints")
    val C: Int by context
    val K: Int by context
    val transitionDestination: IntVarArray by context
    // Note: stateUsed must be [C], but originally it is [M,C]. Be sure to convert!
    val stateUsed: BoolVarArray by context
    val bfsTransitionAutomaton = newBoolVarArray(C, C)
    val bfsParentAutomaton = newIntVarArray(C) { (c) -> 0 until c }

    comment("bfs_t definition")
    // t[i, j] <=> OR_k( transition[i, k, j] )
    for (j in 1..C)
        for (i in 1..C)
            iffOr(bfsTransitionAutomaton[i, j], sequence {
                for (k in 1..K)
                    yield(transitionDestination[i, k] eq j)
            })

    comment("bfs_p definition")
    // (p[j] = i) <=> t[i, j] & AND_{r<i}( ~t[r, j] ) :: 1<=i<j
    for (j in 1..C)
        for (i in 1 until j)
            iffAnd(bfsParentAutomaton[j] eq i, sequence {
                yield(bfsTransitionAutomaton[i, j])
                for (r in 1 until i)
                    yield(-bfsTransitionAutomaton[r, j])
            })
    imply(-stateUsed[1], bfsParentAutomaton[1] eq 0)
    for (j in 2..C)
        iff(
            -stateUsed[j],
            bfsParentAutomaton[j] eq 0
        )

    comment("BFS(p)")
    // (p[j] = i) => (p[j+1] != r) :: LB<=r<i<j<UB
    for (j in 3 until C)
        for (i in 2 until j)
            for (r in 1 until i)
                imply(
                    bfsParentAutomaton[j] eq i,
                    bfsParentAutomaton[j + 1] neq r
                )
}
