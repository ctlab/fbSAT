package ru.ifmo.fbsat.core.task.distributed.complete

import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.solver.forEachModularContext
import ru.ifmo.fbsat.core.task.single.complete.declareCompleteVariables
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.project

private val logger = MyLogger {}

@Suppress("LocalVariableName")
fun Solver.declareDistributedCompleteVariables(
    negativeCompoundScenarioTree: NegativeCompoundScenarioTree,
) {
    context["negativeCompoundScenarioTree"] = negativeCompoundScenarioTree
    context["negCompoundScenarioTree"] = negativeCompoundScenarioTree
    context["negativeCompoundTree"] = negativeCompoundScenarioTree
    context["negCompoundTree"] = negativeCompoundScenarioTree
    context["forbiddenLoops"] = mutableSetOf<Pair<Int, Int>>()

    /* Modular */
    forEachModularContext { m ->
        declareCompleteVariables(
            negativeScenarioTree = negativeCompoundScenarioTree.project(m)
        )
    }

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        logger.warn("Dumping of DistributedCompleteVariables to CNF is not implemented yet")
    }
}
