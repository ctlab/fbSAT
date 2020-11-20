@file:Suppress("LocalVariableName")

package ru.ifmo.fbsat.core.constraints

import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.forEachModularContext
import ru.ifmo.fbsat.core.solver.iff
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newIntVarArray

fun Solver.declareAutomatonBfsConstraints() {
    comment("Automaton BFS constraints")
    declareAutomatonBfsConstraintsImpl()
}

fun Solver.declareParallelModularAutomatonBfsConstraints() {
    comment("Parallel modular automaton BFS constraints")
    forEachModularContext { m ->
        comment("Automaton BFS constraints: for module m = $m")
        declareAutomatonBfsConstraints()
    }
}

fun Solver.declareConsecutiveModularAutomatonBfsConstraints() {
    comment("Consecutive modular automaton BFS constraints")
    forEachModularContext { m ->
        comment("Automaton BFS constraints: for module m = $m")
        declareAutomatonBfsConstraints()
    }
}

fun Solver.declareArbitraryModularAutomatonBfsConstraints() {
    comment("Arbitrary modular automaton BFS constraints")
    forEachModularContext { m ->
        comment("Automaton BFS constraints: for module m = $m")
        declareAutomatonBfsConstraintsImpl()
    }
}

fun Solver.declareDistributedAutomatonBfsConstraints() {
    comment("Distributed automaton BFS constraints")
    forEachModularContext { m ->
        comment("Automaton BFS constraints: for module m = $m")
        declareAutomatonBfsConstraintsImpl_stateUsed()
    }
}

private fun Solver.declareAutomatonBfsConstraintsImpl() {
    val C: Int = context["C"]
    val K: Int = context["K"]
    val transitionDestination: IntVarArray = context["transitionDestination"]
    val bfsTransitionAutomaton = newBoolVarArray(C, C)
    val bfsParentAutomaton = newIntVarArray(C) { (c) -> 1 until c }

    comment("bfs_t definition")
    // t[i, j] <=> OR_k( transition[i, k, j] )
    for (j in 1..C)
        for (i in 1..C)
            iffOr(bfsTransitionAutomaton[i, j]) {
                for (k in 1..K)
                    yield(transitionDestination[i, k] eq j)
            }

    comment("bfs_p definition")
    // (p[j] = i) <=> t[i, j] & AND_{r<i}( ~t[r, j] ) :: i<j
    for (j in 1..C)
        for (i in 1 until j)
            iffAnd(bfsParentAutomaton[j] eq i) {
                yield(bfsTransitionAutomaton[i, j])
                for (r in 1 until i)
                    yield(-bfsTransitionAutomaton[r, j])
            }

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

private fun Solver.declareAutomatonBfsConstraintsImpl_stateUsed() {
    comment("Automaton BFS constraints")
    val C: Int = context["C"]
    val K: Int = context["K"]
    val transitionDestination: IntVarArray = context["transitionDestination"]
    val stateUsed: BoolVarArray = context["stateUsed"]
    val bfsTransitionAutomaton = newBoolVarArray(C, C)
    val bfsParentAutomaton = newIntVarArray(C) { (c) -> 0 until c }

    comment("bfs_t definition")
    // t[i, j] <=> OR_k( transition[i, k, j] )
    for (j in 1..C)
        for (i in 1..C)
            iffOr(bfsTransitionAutomaton[i, j]) {
                for (k in 1..K)
                    yield(transitionDestination[i, k] eq j)
            }

    comment("bfs_p definition")
    // (p[j] = i) <=> t[i, j] & AND_{r<i}( ~t[r, j] ) :: 1<=i<j
    for (j in 1..C)
        for (i in 1 until j)
            iffAnd(bfsParentAutomaton[j] eq i) {
                yield(bfsTransitionAutomaton[i, j])
                for (r in 1 until i)
                    yield(-bfsTransitionAutomaton[r, j])
            }
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
