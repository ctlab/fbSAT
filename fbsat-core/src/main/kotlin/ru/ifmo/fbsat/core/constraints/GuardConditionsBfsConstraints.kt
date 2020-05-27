package ru.ifmo.fbsat.core.constraints

import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.ConsecutiveModularExtendedVariables
import ru.ifmo.fbsat.core.task.modular.extended.parallel.ParallelModularExtendedVariables
import ru.ifmo.fbsat.core.task.single.extended.ExtendedVariables

fun Solver.declareGuardConditionsBfsConstraints(extendedVars: ExtendedVariables) {
    comment("Guard conditions BFS constraints")
    with(extendedVars) {
        for (c in 1..C)
            for (k in 1..K) {
                comment("Guard conditions BFS constraints: for c = $c, k = $k")
                for (j in 3 until P)
                    for (i in 2 until j)
                        for (s in 1 until i)
                            imply(
                                nodeParent[c, k, j] eq i,
                                nodeParent[c, k, j + 1] neq s
                            )
            }
    }
}

fun Solver.declareParallelModularGuardConditionsBfsConstraints(
    parallelModularExtendedVariables: ParallelModularExtendedVariables
) {
    comment("Parallel modular guard conditions BFS constraints")
    with(parallelModularExtendedVariables) {
        for (m in 1..M) {
            comment("Parallel modular guard conditions BFS constraints: for module m = $m")
            declareGuardConditionsBfsConstraints(
                extendedVars = modularExtendedVariables[m]
            )
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
                extendedVars = modularExtendedVariables[m]
            )
        }
    }
}
