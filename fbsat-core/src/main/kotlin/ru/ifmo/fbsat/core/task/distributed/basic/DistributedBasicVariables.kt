package ru.ifmo.fbsat.core.task.distributed.basic

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.basic.declareBasicVariables

class DistributedBasicVariables(
    /* Constants */
    val M: Int,
    val modularScenarioTree: MultiArray<ScenarioTree>,
    val modularC: MultiArray<Int>,
    val modularK: MultiArray<Int>,
    val modularV: MultiArray<Int>,
    val modularE: MultiArray<Int>,
    val modularO: MultiArray<Int>,
    val modularX: MultiArray<Int>,
    val modularZ: MultiArray<Int>,
    val modularU: MultiArray<Int>,
    /* Modularized BasicVariables */
    val modularBasicVariables: MultiArray<BasicVariables>,
    /* Cardinality */
    val cardinality: Cardinality
)

@Suppress("LocalVariableName")
fun Solver.declareDistributedBasicVariables(
    M: Int,
    modularScenarioTree: MultiArray<ScenarioTree>,
    modularC: MultiArray<Int>,
    modularK: MultiArray<Int>,
    modularV: MultiArray<Int> = modularScenarioTree.map { it.size },
    modularE: MultiArray<Int> = modularScenarioTree.map { it.inputEvents.size },
    modularO: MultiArray<Int> = modularScenarioTree.map { it.outputEvents.size },
    modularX: MultiArray<Int> = modularScenarioTree.map { it.inputNames.size },
    modularZ: MultiArray<Int> = modularScenarioTree.map { it.outputNames.size },
    modularU: MultiArray<Int> = modularScenarioTree.map { it.uniqueInputs.size }
): DistributedBasicVariables {
    /* Modularized BasicVariables */
    val modularBasicVariables = MultiArray.create(M) { (m) ->
        declareBasicVariables(
            scenarioTree = modularScenarioTree[m],
            C = modularC[m],
            K = modularK[m]
        )
    }
    /* Cardinality */
    val cardinality = declareCardinality {
        for (m in 1..M) with(modularBasicVariables[m]) {
            for (c in 1..C)
                for (k in 1..K)
                    yield(transitionDestination[c, k] neq 0)
        }
    }

    return DistributedBasicVariables(
        M = M,
        modularScenarioTree = modularScenarioTree,
        modularC = modularC,
        modularK = modularK,
        modularV = modularV,
        modularE = modularE,
        modularO = modularO,
        modularX = modularX,
        modularZ = modularZ,
        modularU = modularU,
        modularBasicVariables = modularBasicVariables,
        cardinality = cardinality
    )
}
