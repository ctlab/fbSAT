@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.core.constraints

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.scenario.ScenarioTreeInterface
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.implyImply
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.complete.CompleteVariables

fun Solver.declarePositiveMappingConstraints(
    basicVariables: BasicVariables,
    isEncodeReverseImplication: Boolean
) {
    comment("Positive mapping constraints")
    with(basicVariables) {
        comment("Positive mapping constraints: for root")
        declareMappingConstraintsForRoot(mapping = mapping)

        for (v in scenarioTree.activeVertices) {
            comment("Positive mapping constraints: for active node v = $v")
            declareMappingConstraintsForActiveNode(
                v = v,
                tree = scenarioTree,
                C = C, Z = Z,
                stateOutputEvent = stateOutputEvent,
                stateAlgorithmTop = stateAlgorithmTop,
                stateAlgorithmBot = stateAlgorithmBot,
                actualTransitionFunction = actualTransitionFunction,
                mapping = mapping
            )
        }

        for (v in scenarioTree.passiveVertices) {
            comment("Positive mapping constraints: for passive node v = $v")
            declareMappingConstraintsForPassiveNode(
                v = v,
                tree = scenarioTree,
                C = C,
                actualTransitionFunction = actualTransitionFunction,
                mapping = mapping
            )
        }

        if (isEncodeReverseImplication) {
            // OR_k(transitionDestination[i,k,j]) => OR_{v|active}( mapping[tp(v), i] & mapping[v, j] )
            for (i in 1..C)
                for (j in 1..C) {
                    val lhsAux = newVariable()
                    iffOr(lhsAux, sequence {
                        for (k in 1..K)
                            yield(transitionDestination[i, k, j])
                    })

                    val rhsAux = newVariable()
                    iffOr(rhsAux, sequence {
                        for (v in scenarioTree.activeVertices) {
                            val p = scenarioTree.parent(v)
                            val aux = newVariable()
                            iffAnd(aux, mapping[p, i], mapping[v, j])
                            yield(aux)
                        }
                    })

                    imply(lhsAux, rhsAux)
                }
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
            declareMappingConstraintsForRoot(mapping = negMapping)
        }

        // Note: be very careful with positive/negative variables!
        for (v in negativeScenarioTree.activeVertices) {
            comment("Negative mapping constraints: for active node v = $v")
            declareMappingConstraintsForActiveNode(
                v = v,
                tree = negativeScenarioTree,
                C = C, Z = Z,
                stateOutputEvent = stateOutputEvent,
                stateAlgorithmTop = stateAlgorithmTop,
                stateAlgorithmBot = stateAlgorithmBot,
                actualTransitionFunction = negActualTransitionFunction,
                mapping = negMapping
            )
        }

        // Note: be very careful with positive/negative variables!
        for (v in negativeScenarioTree.passiveVertices) {
            comment("Negative mapping constraints: for passive node v = $v")
            declareMappingConstraintsForPassiveNode(
                v = v,
                tree = negativeScenarioTree,
                C = C,
                actualTransitionFunction = negActualTransitionFunction,
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
    stateOutputEvent: IntMultiArray,
    stateAlgorithmTop: IntMultiArray,
    stateAlgorithmBot: IntMultiArray,
    actualTransitionFunction: IntMultiArray,
    mapping: IntMultiArray
) {
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)
    val o = tree.outputEvent(v)

    comment("Constraining actual transition function for active node v = $v")
    // actualTransitionFunction[mapping[tp(v)], tie(v), tin(v)] = mapping[v]
    // Note: we need one-way implication here: (mapping = c) => (actualTransitionFunction = c)
    for (i in 1..C)
        for (j in 1..C)
            implyImply(
                mapping[p, i],
                mapping[v, j],
                actualTransitionFunction[i, e, u, j]
            )

    comment("Constraining output event for active node v = $v")
    // stateOutputEvent[mapping[v]] = toe(v)
    for (c in 1..C)
        imply(
            mapping[v, c],
            stateOutputEvent[c, o]
        )

    comment("Constraining algorithms for active node v = $v")
    // stateAlgorithm{tov(p,z)}[mapping[v], z] = tov(v,z)
    for (z in 1..Z) {
        val oldValue = tree.outputValue(p, z)
        val newValue = tree.outputValue(v, z)
        for (c in 1..C)
            imply(
                mapping[v, c],
                when (val values = oldValue to newValue) {
                    true to true -> stateAlgorithmTop[c, z]
                    true to false -> -stateAlgorithmTop[c, z]
                    false to true -> stateAlgorithmBot[c, z]
                    false to false -> -stateAlgorithmBot[c, z]
                    else -> error("Weird combination of values: $values")
                }
            )
    }
}

fun Solver.declareMappingConstraintsForPassiveNode(
    v: Int,
    tree: ScenarioTreeInterface,
    C: Int,
    actualTransitionFunction: IntMultiArray,
    mapping: IntMultiArray
) {
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)

    comment("Mapping propagation for passive node v = $v")
    // mapping[v] = mapping[p]
    for (c in 1..C)
        imply(
            mapping[p, c],
            mapping[v, c]
        )

    comment("Constraining actual transition function for passive node v = $v")
    // actualTransitionFunction[mapping[tp(v)], tie(v), tin(v)] = 0
    for (c in 1..C)
        imply(
            mapping[p, c],
            actualTransitionFunction[c, e, u, C + 1]
        )
}
