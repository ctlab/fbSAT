package ru.ifmo.fbsat.core.constraints

import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.Solver.Companion.falseVariable
import ru.ifmo.fbsat.core.solver.exactlyOne
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.ConsecutiveModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.basic.parallel.ParallelModularBasicVariables
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables

fun Solver.declareAutomatonBfsConstraints(basicVars: BasicVariables) {
    comment("Automaton BFS constraints")
    with(basicVars) {
        val bfsTransitionAutomaton = newArray(C, C)
        val bfsParentAutomaton = newArray(C, C) { (j, i) ->
            if (i < j) newVariable() else falseVariable
        }

        comment("bfs_t definition")
        // t[i, j] <=> OR_k( transition[i, k, j] )
        for (j in 1..C)
            for (i in 1..C)
                iffOr(bfsTransitionAutomaton[i, j], sequence {
                    for (k in 1..K)
                        yield(transitionDestination[i, k, j])
                })

        comment("bfs_p definition")
        // p[j, i] <=> t[i, j] & AND_{k<i}( ~t[k, j] ) :: i<j
        for (j in 1..C)
            for (i in 1 until j)
                iffAnd(bfsParentAutomaton[j, i], sequence {
                    yield(bfsTransitionAutomaton[i, j])
                    for (k in 1 until i)
                        yield(-bfsTransitionAutomaton[k, j])
                })

        comment("EO(p)")
        // EO_{i<j}( p[j,i] ) :: j>1
        for (j in 2..C)
            exactlyOne {
                for (i in 1 until j)
                    yield(bfsParentAutomaton[j, i])
            }

        comment("BFS(p)")
        // p[j, i] => ~p[j+1, k] :: LB<=k<i<j<UB
        for (j in 3 until C)
            for (i in 2 until j)
                for (k in 1 until i)
                    imply(bfsParentAutomaton[j, i], -bfsParentAutomaton[j + 1, k])
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
