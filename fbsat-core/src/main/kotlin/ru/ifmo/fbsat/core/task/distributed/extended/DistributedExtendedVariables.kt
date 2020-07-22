package ru.ifmo.fbsat.core.task.distributed.extended

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.task.distributed.basic.DistributedBasicVariables
import ru.ifmo.fbsat.core.task.single.extended.ExtendedVariables
import ru.ifmo.fbsat.core.task.single.extended.declareExtendedVariables
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log

class DistributedExtendedVariables(
    /* Constants */
    val M: Int,
    val modularScenarioTree: MultiArray<PositiveScenarioTree>,
    val modularC: MultiArray<Int>,
    val modularK: MultiArray<Int>,
    val modularP: MultiArray<Int>,
    val modularV: MultiArray<Int>,
    val modularE: MultiArray<Int>,
    val modularO: MultiArray<Int>,
    val modularX: MultiArray<Int>,
    val modularZ: MultiArray<Int>,
    val modularU: MultiArray<Int>,
    /* Modularized ExtendedVariables */
    val modularExtendedVariables: MultiArray<ExtendedVariables>,
    /* Cardinality */
    val cardinality: Cardinality
)

fun Solver.declareDistributedExtendedVariables(
    basicVars: DistributedBasicVariables,
    modularP: MultiArray<Int>
): DistributedExtendedVariables = with(basicVars) {
    /* Modularized ExtendedVariables */
    val modularExtendedVariables = MultiArray.create(M) { (m) ->
        declareExtendedVariables(modularBasicVariables[m], P = modularP[m])
    }
    /* Cardinality */
    val cardinality = declareCardinality {
        for (m in 1..M) with(modularExtendedVariables[m]) {
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        yield(nodeType[c, k, p] neq NodeType.NONE)
        }
    }

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        log.warn("Dump of variables to CNF is not implemented yet")
    }

    DistributedExtendedVariables(
        M = M,
        modularScenarioTree = modularScenarioTree,
        modularC = modularC,
        modularK = modularK,
        modularP = modularP,
        modularV = modularV,
        modularE = modularE,
        modularO = modularO,
        modularX = modularX,
        modularZ = modularZ,
        modularU = modularU,
        modularExtendedVariables = modularExtendedVariables,
        cardinality = cardinality
    )
}
