package ru.ifmo.fbsat.core.task.distributed.complete

import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareModularContext
import ru.ifmo.fbsat.core.solver.switchContext
import ru.ifmo.fbsat.core.task.single.complete.declareCompleteVariables
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.project

fun Solver.declareDistributedCompleteVariables(
    negativeCompoundScenarioTree: NegativeCompoundScenarioTree
) {
    val M: Int by context

    /* Modular */
    val modularContext = declareModularContext(M)
    for (m in 1..M) switchContext(modularContext[m]) {
        declareCompleteVariables(
            negativeScenarioTree = negativeCompoundScenarioTree.project(m)
        )
    }

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        log.warn("Dump of CompleteVariables to CNF is not implemented yet")
    }
}
