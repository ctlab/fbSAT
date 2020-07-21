package ru.ifmo.fbsat.core.task.distributed.complete

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.negative.toOld
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.distributed.extended.DistributedExtendedVariables
import ru.ifmo.fbsat.core.task.single.complete.CompleteVariables
import ru.ifmo.fbsat.core.task.single.complete.declareCompleteVariables
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log

class DistributedCompleteVariables(
    /* Constants */
    val M: Int,
    val modularScenarioTree: MultiArray<PositiveScenarioTree>,
    val negativeCompoundScenarioTree: NegativeCompoundScenarioTree,
    val modularC: MultiArray<Int>,
    val modularK: MultiArray<Int>,
    val modularP: MultiArray<Int>,
    val modularV: MultiArray<Int>,
    val modularE: MultiArray<Int>,
    val modularO: MultiArray<Int>,
    val modularX: MultiArray<Int>,
    val modularZ: MultiArray<Int>,
    val modularU: MultiArray<Int>,
    /* Modularized CompleteVariables */
    val modularCompleteVariables: MultiArray<CompleteVariables>
) {
    val forbiddenLoops: MutableSet<Pair<Int, Int>> = mutableSetOf()
}

fun Solver.declareDistributedCompleteVariables(
    extendedVars: DistributedExtendedVariables,
    negativeCompoundScenarioTree: NegativeCompoundScenarioTree
): DistributedCompleteVariables = with(extendedVars) {
    /* Modularized CompleteVariables */
    val modularCompleteVariables = MultiArray.create(M) { (m) ->
        declareCompleteVariables(
            extendedVars = modularExtendedVariables[m],
            negativeScenarioTree = negativeCompoundScenarioTree.modular[m].toOld()
        )
    }

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        log.warn("Dump of CompleteVariables to CNF is not implemented yet")
    }

    DistributedCompleteVariables(
        M = M,
        modularScenarioTree = modularScenarioTree,
        negativeCompoundScenarioTree = negativeCompoundScenarioTree,
        modularC = modularC,
        modularK = modularK,
        modularP = modularP,
        modularV = modularV,
        modularE = modularE,
        modularO = modularO,
        modularX = modularX,
        modularZ = modularZ,
        modularU = modularU,
        modularCompleteVariables = modularCompleteVariables
    )
}
