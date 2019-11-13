package ru.ifmo.fbsat.core.constraints

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.scenario.ScenarioTreeInterface
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.implyIff
import ru.ifmo.fbsat.core.solver.implyIffAnd
import ru.ifmo.fbsat.core.solver.implyOr
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.complete.CompleteVariables
import ru.ifmo.fbsat.core.utils.algorithmChoice

fun Solver.declarePositiveMappingConstraints(
    basicVariables: BasicVariables,
    isEncodeReverseImplication: Boolean
) {
    comment("Positive mapping constraints")
    with(basicVariables) {
        /* Constraints for the root */
        comment("Positive mapping constraints: for root")
        declareMappingConstraintsForRoot(mapping = mapping)

        /* Constraints for active vertices */
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

        /* Constraints for passive vertices */
        for (v in scenarioTree.passiveVertices) {
            comment("Positive mapping constraints: for passive node v = $v")
            declareMappingConstraintsForPassiveNode(
                v = v,
                tree = scenarioTree,
                C = C, O = O,
                stateOutputEvent = stateOutputEvent,
                actualTransitionFunction = actualTransitionFunction,
                mapping = mapping,
                isPositive = true
            )
        }

        /* Additional constraints */

        if (isEncodeReverseImplication) {
            comment("Mysterious reverse-implication")
            // OR_k(transitionDestination[i,k,j]) => OR_{v|active}( mapping[tp(v),i] & mapping[v,j] )
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
        /* Constraints for the root */
        // Skip constraints for the root if they are already defined
        if (1 in Vs) {
            comment("Negative mapping constraints: for root")
            declareMappingConstraintsForRoot(mapping = negMapping)
        }

        /* Constraints for active vertices */
        // Note: be very careful with positive/negative variables!
        for (v in Vs.intersect(negativeScenarioTree.activeVertices)) {
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

        /* Constraints for passive vertices */
        // Note: be very careful with positive/negative variables!
        for (v in Vs.intersect(negativeScenarioTree.passiveVertices)) {
            comment("Negative mapping constraints: for passive node v = $v")
            declareMappingConstraintsForPassiveNode(
                v = v,
                tree = negativeScenarioTree,
                C = C, O = O,
                stateOutputEvent = stateOutputEvent,
                actualTransitionFunction = negActualTransitionFunction,
                mapping = negMapping,
                isPositive = false
            )
        }

        /* Additional constraints */

        for (v in Vs.filter { it != 1 }) {
            val p = negativeScenarioTree.parent(v)

            comment("Non-satisfaction propagation")
            // (negMapping[tp(v)] = 0) => (negMapping[v] = 0)
            imply(
                negMapping[p, C + 1],
                negMapping[v, C + 1]
            )
        }

        comment("Forbid loops")
        // (negMapping[v] = c) => AND_{l in loopBacks(v)}(negMapping[l] != c)
        for (v in 1..negV)
            for (l in negativeScenarioTree.loopBacks(v))
                if (forbiddenLoops.add(v to l))
                    for (c in 1..C)
                        imply(
                            negMapping[v, c],
                            -negMapping[l, c]
                        )
    }
}

private fun Solver.declareMappingConstraintsForRoot(
    mapping: IntMultiArray
) {
    comment("Root maps to the initial state")
    clause(mapping[1, 1])
}

private fun Solver.declareMappingConstraintsForActiveNode(
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

    comment("Mapping definition for active node v = $v")
    // (mapping[v] = c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)] = c) & (stateOutputEvent[c] = toe(v)) & AND_{z}(stateAlgorithm{tov(tp(v),z)}(c,z) = tov(v,z))
    for (i in 1..C)
        for (j in 1..C)
            implyIffAnd(
                mapping[p, i],
                mapping[v, j],
                sequence {
                    yield(actualTransitionFunction[i, e, u, j])
                    yield(stateOutputEvent[j, o])
                    for (z in 1..Z)
                        yield(
                            algorithmChoice(
                                tree = tree,
                                v = v, c = j, z = z,
                                algorithmTop = stateAlgorithmTop,
                                algorithmBot = stateAlgorithmBot
                            )
                        )
                }
            )
}

private fun Solver.declareMappingConstraintsForPassiveNode(
    v: Int,
    tree: ScenarioTreeInterface,
    C: Int,
    O: Int,
    stateOutputEvent: IntMultiArray,
    actualTransitionFunction: IntMultiArray,
    mapping: IntMultiArray,
    isPositive: Boolean
) {
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)

    if (isPositive) {
        comment("Mapping propagation for passive node v = $v")
        // mapping[v] = mapping[tp(v)]
        for (c in 1..C)
            imply(
                mapping[p, c],
                mapping[v, c]
            )

        comment("Constraining actualTransitionFunction for passive node v = $v")
        // actualTransition[mapping[tp(v)],tie(v),tin(v)] = 0
        for (c in 1..C)
            imply(
                mapping[p, c],
                actualTransitionFunction[c, e, u, C + 1]
            )
    } else {
        comment("Mapping propagation for passive node v = $v")
        // (negMapping[v] = negMapping[tp(v)]) | (negMapping[v] = 0)
        for (c in 1..C)
            implyOr(
                mapping[p, c],
                mapping[v, c],
                mapping[v, C + 1]
            )

        comment("Constraining actualTransitionFunction for passive node v = $v")
        // (negMapping[v] != 0) => (negActualTransition[mapping[tp(v)],tie(v),tin(v)] = 0)
        for (c in 1..C)
            implyIff(
                mapping[p, c],
                mapping[v, c],
                actualTransitionFunction[c, e, u, C + 1]
            )
    }
}
