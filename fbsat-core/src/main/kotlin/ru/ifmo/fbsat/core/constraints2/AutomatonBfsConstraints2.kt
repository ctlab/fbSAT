package ru.ifmo.fbsat.core.constraints2

import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newIntVarArray
import ru.ifmo.fbsat.core.task.single.basic2.BasicVariables2

fun Solver.declareAutomatonBfsConstraints2(basicVars: BasicVariables2) {
    with(basicVars) {
        declareAutomatonBfsConstraintsImpl2(
            C = C, K = K,
            transitionDestination = transitionDestination
        )
    }
}

private fun Solver.declareAutomatonBfsConstraintsImpl2(
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
