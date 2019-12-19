package ru.ifmo.fbsat.core.constraints

import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.Solver.Companion.falseVariable
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.ConsecutiveModularExtendedVariables
import ru.ifmo.fbsat.core.task.single.extended.ExtendedVariables

fun Solver.declareGuardConditionsBfsConstraints(extendedVars: ExtendedVariables) {
    comment("Guard conditions BFS constraints")
    with(extendedVars) {
        for (c in 1..C)
            for (k in 1..K) {
                val bfsTransitionGuard = newBoolVar(P, P) { (i, j) ->
                    if (j > i) nodeParent[c, k, j] eq i else falseVariable
                }
                val bfsParentGuard = newIntVar(P) { (j) -> 0 until j }

                comment("F_p")
                // (p[j] = i) <=> t[i, j] & AND_{k<i}( ~t[k, j] ) :: i<j
                for (j in 1..P)
                    for (i in 1 until j)
                        iffAnd(bfsParentGuard[j] eq i, sequence {
                            yield(bfsTransitionGuard[i, j])
                            for (n in 1 until i)
                                yield(-bfsTransitionGuard[n, j])
                        })

                comment("F_BFS(p)")
                // p[j, i] => ~p[j+1, n] :: LB<=n<i<j<UB
                for (j in 1 until P)
                    for (i in 1 until j)
                        for (n in 1 until i)
                            imply(bfsParentGuard[j] eq i, bfsParentGuard[j + 1] neq n)
            }
    }
}

fun Solver.declareConsecutiveModularGuardConditionsBfsConstraints(
    consecutiveModularExtendedVariables: ConsecutiveModularExtendedVariables
) {
    comment("Consecutive modular guard conditions BFS constraints")
    with(consecutiveModularExtendedVariables) {
        for (m in 1..M) {
            comment("Consecutive modular guard conditions BFS constraints: for module m = $m")
            declareGuardConditionsBfsConstraints(
                modularExtendedVariables[m]
            )
        }
    }
}
