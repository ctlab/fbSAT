package ru.ifmo.fbsat.core.constraints

import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.Solver.Companion.falseVariable
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.ConsecutiveModularExtendedVariables
import ru.ifmo.fbsat.core.task.single.extended.ExtendedVariables

fun Solver.declareGuardConditionsBfsConstraints(extendedVars: ExtendedVariables) {
    println("Guard conditions BFS constraints")
    with(extendedVars) {
        for (c in 1..C)
            for (k in 1..K) {
                val bfsTransitionGuard = newArray(P, P) { (i, j) ->
                    if (j > i) nodeParent[c, k, j, i] else falseVariable
                }
                val bfsParentGuard = newArray(P, P) { (j, i) ->
                    if (i < j) newVariable() else falseVariable
                }

                comment("F_p")
                // p[j, i] <=> t[i, j] & AND_{n<i}( ~t[n, j] )
                for (j in 1..P)
                    for (i in 1 until j)
                        iffAnd(bfsParentGuard[j, i], sequence {
                            yield(bfsTransitionGuard[i, j])
                            for (n in 1 until i)
                                yield(-bfsTransitionGuard[n, j])
                        })

                comment("F_BFS(p)")
                // p[j, i] => ~p[j+1, n] :: LB<=n<i<j<UB
                for (j in 1 until P)
                    for (i in 1 until j)
                        for (n in 1 until i)
                            imply(bfsParentGuard[j, i], -bfsParentGuard[j + 1, n])
            }
    }
}

fun Solver.declareConsecutiveModularGuardConditionsBfsConstraints(
    consecutiveModularExtendedVariables: ConsecutiveModularExtendedVariables
) {
    println("Consecutive modular guard conditions BFS constraints")
    with(consecutiveModularExtendedVariables) {
        for (m in 1..M) {
            println("Consecutive modular guard conditions BFS constraints: for module m = $m")
            declareGuardConditionsBfsConstraints(
                modularExtendedVariables[m]
            )
        }
    }
}
