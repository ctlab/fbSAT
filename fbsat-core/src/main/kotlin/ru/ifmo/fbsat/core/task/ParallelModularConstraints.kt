@file:Suppress("LocalVariableName", "ReplaceRangeToWithUntil")

package ru.ifmo.fbsat.core.task

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.atLeastOne
import ru.ifmo.fbsat.core.solver.exactlyOne
import ru.ifmo.fbsat.core.solver.implyAnd
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.StartStateAlgorithms

fun Solver.declareParallelModularColorConstraints(
    scenarioTree: ScenarioTree,
    M: Int,
    C: Int,
    K: Int,
    V: Int,
    colorModular: MultiArray<IntMultiArray>,
    transitionModular: MultiArray<IntMultiArray>,
    actualTransitionModular: MultiArray<IntMultiArray>,
    isEncodeReverseImplication: Boolean = true
) {
    for (m in 1..M)
        declareColorConstraints(
            scenarioTree = scenarioTree,
            C = C, K = K, V = V,
            color = colorModular[m],
            transition = transitionModular[m],
            actualTransition = actualTransitionModular[m],
            isEncodeReverseImplication = isEncodeReverseImplication
        )
}

fun Solver.declareParallelModularTransitionConstraints(
    M: Int,
    C: Int,
    K: Int,
    E: Int,
    U: Int,
    transitionModular: MultiArray<IntMultiArray>,
    actualTransitionModular: MultiArray<IntMultiArray>,
    inputEventModular: MultiArray<IntMultiArray>,
    firstFiredModular: MultiArray<IntMultiArray>
) {
    for (m in 1..M)
        declareTransitionConstraints(
            C = C, K = K, E = E, U = U,
            transition = transitionModular[m],
            actualTransition = actualTransitionModular[m],
            inputEvent = inputEventModular[m],
            firstFired = firstFiredModular[m]
        )
}

fun Solver.declareParallelModularFiringConstraints(
    M: Int,
    C: Int,
    K: Int,
    U: Int,
    rootValueModular: MultiArray<IntMultiArray>,
    firstFiredModular: MultiArray<IntMultiArray>,
    notFiredModular: MultiArray<IntMultiArray>
) {
    for (m in 1..M)
        declareFiringConstraints(
            C = C, K = K, U = U,
            rootValue = rootValueModular[m],
            firstFired = firstFiredModular[m],
            notFired = notFiredModular[m]
        )
}

fun Solver.declareParallelModularOutputEventConstraints(
    scenarioTree: ScenarioTree,
    M: Int,
    C: Int,
    O: Int,
    colorModular: MultiArray<IntMultiArray>,
    outputEventModular: MultiArray<IntMultiArray>
) {
    for (m in 1..M)
        declareOutputEventConstraints(
            scenarioTree = scenarioTree,
            C = C, O = O,
            color = colorModular[m],
            outputEvent = outputEventModular[m]
        )
}

fun Solver.declareParallelModularAlgorithmConstraints(
    scenarioTree: ScenarioTree,
    M: Int,
    C: Int,
    Z: Int,
    outputVariableModule: IntMultiArray,
    algorithm0Modular: MultiArray<IntMultiArray>,
    algorithm1Modular: MultiArray<IntMultiArray>,
    colorModular: MultiArray<IntMultiArray>
) {

    // EO
    for (z in 1..Z)
        exactlyOne {
            for (m in 1..M)
                yield(outputVariableModule[z, m])
        }
    // ALO
    for (m in 1..M)
        atLeastOne {
            for (z in 1..Z)
                yield(outputVariableModule[z, m])
        }

    comment("5. Algorithm constraints")

    when (Globals.START_STATE_ALGORITHMS) {
        StartStateAlgorithms.NOTHING -> {
            comment("5.1. Start state does nothing")
            for (m in 1..M)
                for (z in 1..Z) {
                    clause(-algorithm0Modular[m][1, z])
                    clause(algorithm1Modular[m][1, z])
                }
        }
        StartStateAlgorithms.ZERO -> {
            comment("5.1. Start state produces zeros")
            for (m in 1..M)
                for (z in 1..Z) {
                    clause(-algorithm0Modular[m][1, z])
                    clause(-algorithm1Modular[m][1, z])
                }
        }
        StartStateAlgorithms.ARBITRARY -> TODO("Arbitrary start state algorithms")
    }

    comment("5.2. Algorithms definition")
    for (v in scenarioTree.activeVertices) {
        val p = scenarioTree.parent(v)
        for (z in 1..Z) {
            val oldValue = scenarioTree.outputValue(p, z)
            val newValue = scenarioTree.outputValue(v, z)
            for (m in 1..M)
                for (c in 1..C)
                    clause(
                        -outputVariableModule[z, m],
                        -colorModular[m][v, c],
                        when (val values = oldValue to newValue) {
                            false to false -> -algorithm0Modular[m][c, z]
                            false to true -> algorithm0Modular[m][c, z]
                            true to false -> -algorithm1Modular[m][c, z]
                            true to true -> algorithm1Modular[m][c, z]
                            else -> error("Weird combination of values: $values")
                        }
                    )
        }
    }

    for (m in 1..M)
        for (z in 1..Z)
            for (c in 2..C) {
                implyAnd(-outputVariableModule[z, m], -algorithm0Modular[m][c, z])
                implyAnd(-outputVariableModule[z, m], -algorithm1Modular[m][c, z])
            }
}
