package ru.ifmo.fbsat.core.task.modular.basic.parallel

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.newContext
import ru.ifmo.fbsat.core.solver.newIntVarArray
import ru.ifmo.fbsat.core.solver.switchContext
import ru.ifmo.fbsat.core.task.single.basic.declareBasicVariables

@Suppress("LocalVariableName", "NAME_SHADOWING")
fun Solver.declareParallelModularBasicVariables(
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
    val M by context(M)
    val C by context(C)
    val K by context(K)
    val V by context(V)
    val E by context(E)
    val O by context(O)
    val X by context(X)
    val Z by context(Z)
    val U by context(U)

    /* Modular */
    val modularContext by context(MultiArray.create(M) { newContext() })
    for (m in 1..M) switchContext(modularContext[m]) {
        declareBasicVariables(
            scenarioTree = scenarioTree,
            C = C, K = K,
            V = V, E = E, O = O, X = X, Z = Z, U = U
        )
    }

    /* Interface variables */
    val moduleControllingOutputVariable by context(newIntVarArray(Z) { 1..M })

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
