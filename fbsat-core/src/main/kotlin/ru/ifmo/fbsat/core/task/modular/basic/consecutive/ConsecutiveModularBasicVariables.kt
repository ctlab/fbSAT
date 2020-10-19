package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.newBoolVarArray
import ru.ifmo.fbsat.core.solver.newContext
import ru.ifmo.fbsat.core.solver.switchContext
import ru.ifmo.fbsat.core.task.single.basic.declareBasicVariables

fun Solver.declareConsecutiveModularBasicVariables(
    scenarioTree: PositiveScenarioTree,
    M: Int,
    C: Int,
    K: Int,
    V: Int = scenarioTree.size,
    E: Int = scenarioTree.inputEvents.size,
    O: Int = scenarioTree.outputEvents.size,
    X: Int = scenarioTree.inputNames.size,
    Z: Int = scenarioTree.outputNames.size,
    U: Int = scenarioTree.uniqueInputs.size
) {
    /* Modular */
    val modularContext by context(MultiArray.create(M) { newContext() })
    for (m in 1..M) switchContext(modularContext[m]) {
        declareBasicVariables(
            scenarioTree = scenarioTree,
            C = C,
            K = K,
            V = V,
            E = if (m == 1) E else 1,
            O = if (m == M) O else 1,
            X = X,
            Z = Z,
            U = U
        )
    }

    /* Mapping variables */
    val modularComputedOutputValue by context(MultiArray.create(M) { newBoolVarArray(V, Z) })

    /* Cardinality */
    val cardinalityT by context(declareCardinality {
        for (m in 1..M) switchContext(modularContext[m]) {
            val C: Int by context
            val K: Int by context
            val transitionDestination: IntVarArray by context
            for (c in 1..C)
                for (k in 1..K)
                    yield(transitionDestination[c, k] neq 0)
        }
    })
}
