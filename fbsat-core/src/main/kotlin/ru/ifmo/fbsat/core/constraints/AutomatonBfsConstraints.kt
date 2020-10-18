package ru.ifmo.fbsat.core.constraints

import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.iff
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newIntVarArray
import ru.ifmo.fbsat.core.task.distributed.basic.DistributedBasicVariables
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.ArbitraryModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.ConsecutiveModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.basic.parallel.ParallelModularBasicVariables
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables

fun Solver.declareAutomatonBfsConstraints(basicVars: BasicVariables) {
    with(basicVars) {
        declareAutomatonBfsConstraintsImpl(
            C = C, K = K,
            transitionDestination = transitionDestination
        )
    }
}

fun Solver.declareParallelModularAutomatonBfsConstraints(
    parallelModularBasicVariables: ParallelModularBasicVariables
) {
    comment("Parallel modular automaton BFS constraints")
    with(parallelModularBasicVariables) {
        for (m in 1..M) {
            comment("Automaton BFS constraints: for module m = $m")
            declareAutomatonBfsConstraints(modularBasicVariables[m])
        }
    }
}

fun Solver.declareConsecutiveModularAutomatonBfsConstraints(
    consecutiveModularBasicVariables: ConsecutiveModularBasicVariables
) {
    comment("Consecutive modular automaton BFS constraints")
    with(consecutiveModularBasicVariables) {
        for (m in 1..M) {
            comment("Automaton BFS constraints: for module m = $m")
            declareAutomatonBfsConstraints(modularBasicVariables[m])
        }
    }
}

fun Solver.declareArbitraryModularAutomatonBfsConstraints(
    arbitraryModularBasicVariables: ArbitraryModularBasicVariables
) {
    comment("Arbitrary modular automaton BFS constraints")
    with(arbitraryModularBasicVariables) {
        for (m in 1..M) {
            comment("Automaton BFS constraints: for module m = $m")
            declareAutomatonBfsConstraintsImpl(
                C = C, K = K,
                transitionDestination = modularTransitionDestination[m]
            )
        }
    }
}

fun Solver.declareDistributedAutomatonBfsConstraints(
    distributedBasicVariables: DistributedBasicVariables
) {
    comment("Distributed automaton BFS constraints")
    with(distributedBasicVariables) {
        for (m in 1..M) with(modularBasicVariables[m]) {
            comment("Automaton BFS constraints: for module m = $m")
            declareAutomatonBfsConstraintsImpl_stateUsed(
                C = C, K = K,
                transitionDestination = transitionDestination,
                stateUsed = BoolVarArray.create(C) { (c) -> stateUsed[m, c] }
            )
        }
    }
}

private fun Solver.declareAutomatonBfsConstraintsImpl(
    C: Int,
    K: Int,
    transitionDestination: IntVarArray
) {
    comment("Automaton BFS constraints")
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
    C: Int,
    K: Int,
    transitionDestination: IntVarArray,
    stateUsed: BoolVarArray // Note: this must be [C], but originally it is [M,C]. Be sure to convert!
) {
    comment("Automaton BFS constraints")
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
