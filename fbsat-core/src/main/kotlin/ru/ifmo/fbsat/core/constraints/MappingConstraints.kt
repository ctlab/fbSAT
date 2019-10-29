@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.core.constraints

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTreeInterface
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.implyIffAnd
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.complete.CompleteVariables

fun Solver.declarePositiveMappingConstraints(basicVariables: BasicVariables) {
    comment("Positive mapping constraints")
    with(basicVariables) {
        comment("Positive mapping constraints: for root")
        declareMappingConstraintsForRoot(mapping)

        for (v in 2..V) {
            comment("Positive mapping constraints: for v = $v")
            declareMappingConstraintsForNode(
                v = v,
                tree = scenarioTree,
                C = C, Z = Z,
                transitionFunction = transitionFunction,
                outputEventFunction = outputEventFunction,
                algorithmFunctionTop = algorithmFunctionTop,
                algorithmFunctionBot = algorithmFunctionBot,
                mapping = mapping
            )
        }
    }
}

fun Solver.declareNegativeMappingConstraints(completeVars: CompleteVariables, Vs: Iterable<Int>) {
    comment("Negative mapping constraints")
    with(completeVars) {
        // Skip constraints for the root if they are already defined
        if (1 in Vs) {
            comment("Negative mapping constraints: for root")
            declareMappingConstraintsForRoot(negMapping)
        }

        for (v in Vs.filter { it != 1 }) {
            // Note: be very careful with positive/negative variables!
            comment("Negative mapping constraints: for v = $v")
            declareMappingConstraintsForNode(
                v = v,
                tree = scenarioTree,
                C = C, Z = Z,
                transitionFunction = negTransitionFunction,
                outputEventFunction = negOutputEventFunction,
                algorithmFunctionTop = negAlgorithmFunctionTop,
                algorithmFunctionBot = negAlgorithmFunctionBot,
                mapping = negMapping
            )
        }
    }
}

fun Solver.declareMappingConstraintsForRoot(
    mapping: IntMultiArray
) {
    comment("Root maps to the initial state")
    clause(mapping[1, 1])
}

fun Solver.declareMappingConstraintsForNode(
    v: Int,
    tree: ScenarioTreeInterface,
    C: Int,
    Z: Int,
    transitionFunction: IntMultiArray,
    outputEventFunction: IntMultiArray,
    algorithmFunctionTop: IntMultiArray,
    algorithmFunctionBot: IntMultiArray,
    mapping: IntMultiArray
) {
    comment("Mapping definition for v = $v")
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)
    val o = tree.outputEvent(v)
    for (i in 1..C)
        for (j in 1..C) {
            implyIffAnd(mapping[p, i], mapping[v, j], sequence {
                yield(transitionFunction[i, e, u, j])
                yield(outputEventFunction[i, e, u, o])
                for (z in 1..Z) {
                    val oldValue = tree.outputValue(p, z)
                    val newValue = tree.outputValue(v, z)
                    yield(
                        when (val values = oldValue to newValue) {
                            true to true -> algorithmFunctionTop[i, e, u, z]
                            true to false -> -algorithmFunctionTop[i, e, u, z]
                            false to true -> algorithmFunctionBot[i, e, u, z]
                            false to false -> -algorithmFunctionBot[i, e, u, z]
                            else -> error("Weird combination of values: $values")
                        }
                    )
                }
            })
        }
}
