package ru.ifmo.fbsat.core.task.distributed.basic

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.declareModularContext
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.switchContext
import ru.ifmo.fbsat.core.task.single.basic.declareBasicVariables

@Suppress("LocalVariableName", "NAME_SHADOWING")
fun Solver.declareDistributedBasicVariables(
    M: Int,
    compoundScenarioTree: PositiveCompoundScenarioTree,
    modularScenarioTree: MultiArray<PositiveScenarioTree> = compoundScenarioTree.modular,
    modularC: MultiArray<Int>,
    modularK: MultiArray<Int>,
    modularV: MultiArray<Int> = compoundScenarioTree.modular.map { it.size },
    modularE: MultiArray<Int> = compoundScenarioTree.modular.map { it.inputEvents.size },
    modularO: MultiArray<Int> = compoundScenarioTree.modular.map { it.outputEvents.size },
    modularX: MultiArray<Int> = compoundScenarioTree.modular.map { it.inputNames.size },
    modularZ: MultiArray<Int> = compoundScenarioTree.modular.map { it.outputNames.size },
    modularU: MultiArray<Int> = compoundScenarioTree.modular.map { it.uniqueInputs.size }
) {
    val M by context(M)
    val compoundScenarioTree by context(compoundScenarioTree)
    val modularScenarioTree by context(modularScenarioTree)
    val modularC by context(modularC)
    val modularK by context(modularK)
    val modularV by context(modularV)
    val modularE by context(modularE)
    val modularO by context(modularO)
    val modularX by context(modularX)
    val modularZ by context(modularZ)
    val modularU by context(modularU)

    /* Modular */
    // val modularContext by context(MultiArray.create(M) { newContext() })
    val modularContext = declareModularContext(M)
    for (m in 1..M) switchContext(modularContext[m]) {
        declareBasicVariables(
            scenarioTree = modularScenarioTree[m],
            C = modularC[m],
            K = modularK[m],
            V = modularV[m],
            E = modularE[m],
            O = modularO[m],
            X = modularX[m],
            Z = modularZ[m],
            U = modularU[m]
        )
    }

    /* Cardinality */
    // require(modularC.values.all { it == modularC[1] }) { "All C must be equal" }
    val falseVar = newLiteral()
    clause(-falseVar)
    val stateUsed = newBoolVarArray(M, modularC.values.max()!!) { (m, c) ->
        if (c <= modularC[m])
            newLiteral()
        else
            falseVar
    }
    val cardinalityC: Cardinality = declareCardinality {
        for (m in 1..M) switchContext(modularContext[m]) {
            // check(C == modularC[1])
            val C: Int by context
            for (c in 1..C)
                yield(stateUsed[m, c])
        }
    }
    val cardinality = declareCardinality {
        for (m in 1..M) switchContext(modularContext[m]) {
            val C: Int by context
            val K: Int by context
            val transitionDestination: IntVarArray by context
            for (c in 1..C)
                for (k in 1..K)
                    yield(transitionDestination[c, k] neq 0)
        }
    }
}
