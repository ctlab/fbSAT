@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.core.constraints

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.scenario.ScenarioTreeInterface
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.implyIff
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.complete.CompleteVariables

fun Solver.declarePositiveMappingConstraints(
    basicVariables: BasicVariables,
    isEncodeReverseImplication: Boolean = true
) {
    comment("Positive mapping constraints")
    with(basicVariables) {
        comment("Positive mapping constraints: for root")
        declareMappingConstraintsForRoot(mapping)

        for (v in scenarioTree.activeVertices) {
            comment("Positive mapping constraints: for active node v = $v")
            declareMappingConstraintsForActiveNode(
                v = v,
                tree = scenarioTree,
                C = C, Z = Z,
                transitionFunction = transitionFunction,
                outputEventFunction = outputEventFunction,
                algorithmFunctionTop = algorithmFunctionTop,
                algorithmFunctionBot = algorithmFunctionBot,
                mapping = mapping,
                isEncodeReverseImplication = false
            )
        }

        for (v in scenarioTree.passiveVertices) {
            comment("Positive mapping constraints: for passive node v = $v")
            declareMappingConstraintsForPassiveNode(
                v = v,
                tree = scenarioTree,
                C = C, O = O,
                transitionFunction = transitionFunction,
                outputEventFunction = outputEventFunction,
                mapping = mapping
            )
        }
    }
}

fun Solver.declareNegativeMappingConstraints(
    completeVars: CompleteVariables,
    Vs: Iterable<Int>
) {
    comment("Negative mapping constraints")
    with(completeVars) {
        // Skip constraints for the root if they are already defined
        if (1 in Vs) {
            comment("Negative mapping constraints: for root")
            declareMappingConstraintsForRoot(negMapping)
        }

        // Note: be very careful with positive/negative variables!
        for (v in negativeScenarioTree.activeVertices) {
            comment("Negative mapping constraints: for active node v = $v")
            declareMappingConstraintsForActiveNode(
                v = v,
                tree = negativeScenarioTree,
                C = C, Z = Z,
                transitionFunction = negTransitionFunction,
                outputEventFunction = negOutputEventFunction,
                algorithmFunctionTop = negAlgorithmFunctionTop,
                algorithmFunctionBot = negAlgorithmFunctionBot,
                mapping = negMapping,
                isEncodeReverseImplication = false
            )
        }

        // Note: be very careful with positive/negative variables!
        for (v in negativeScenarioTree.passiveVertices) {
            comment("Negative mapping constraints: for passive node v = $v")
            declareMappingConstraintsForPassiveNode(
                v = v,
                tree = negativeScenarioTree,
                C = C, O = O,
                transitionFunction = negTransitionFunction,
                outputEventFunction = negOutputEventFunction,
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

fun Solver.declareMappingConstraintsForActiveNode(
    v: Int,
    tree: ScenarioTreeInterface,
    C: Int,
    Z: Int,
    transitionFunction: IntMultiArray,
    outputEventFunction: IntMultiArray,
    algorithmFunctionTop: IntMultiArray,
    algorithmFunctionBot: IntMultiArray,
    mapping: IntMultiArray,
    isEncodeReverseImplication: Boolean
) {
    // if (isEncodeReverseImplication) {
    //     log.warn("'Reverse implication' is not yet implemented")
    // }

    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)
    val o = tree.outputEvent(v)

    comment("Mapping definition for active node v = $v")
    // mapping[v] = transitionFunction[mapping[tp(v)], tie(v), tin(v)]
    for (i in 1..C)
        for (j in 1..C)
            implyIff(
                mapping[p, i],
                mapping[v, j],
                transitionFunction[i, e, u, j]
            )

    comment("Constraining output event function for active node v = $v")
    // outputEventFunction[mapping[tp(v)], tie(v), tin(v)] = toe(v)
    for (c in 1..C)
        imply(
            mapping[p, c],
            outputEventFunction[c, e, u, o]
        )

    comment("Constraining algorithms for active node v = $v")
    // algorithmFunction{tov(tp(v),z)}[mapping[tp(v)], tie(v), tin(v), z] = tov(v,z)
    for (c in 1..C)
        for (z in 1..Z)
            imply(
                mapping[p, c],
                when (val values = tree.outputValue(p, z) to tree.outputValue(v, z)) {
                    true to true -> algorithmFunctionTop[c, e, u, z]
                    true to false -> -algorithmFunctionTop[c, e, u, z]
                    false to true -> algorithmFunctionBot[c, e, u, z]
                    false to false -> -algorithmFunctionBot[c, e, u, z]
                    else -> error("Weird combination of values: $values")
                }
            )
}

fun Solver.declareMappingConstraintsForPassiveNode(
    v: Int,
    tree: ScenarioTreeInterface,
    C: Int,
    O: Int,
    transitionFunction: IntMultiArray,
    outputEventFunction: IntMultiArray,
    mapping: IntMultiArray
) {
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)

    comment("Mapping definition for passive node v = $v")
    // mapping[v] = mapping[p]
    for (c in 1..C)
        imply(
            mapping[p, c],
            mapping[v, c]
        )

    comment("Constraining transition function for passive node v = $v")
    // transitionFunction[mapping[tp(v)], tie(v), tin(v)] = mapping[tp(v)]
    for (c in 1..C)
        imply(
            mapping[p, c],
            transitionFunction[c, e, u, c]
        )

    comment("Constraining output event function for passive node v = $v")
    // outputEventFunction[mapping[tp(v)], tie(v), tin(v)] = epsilon
    for (c in 1..C)
        imply(
            mapping[p, c],
            outputEventFunction[c, e, u, O + 1]
        )

    comment("Not constraining algorithms for passive node v = $v")
}
