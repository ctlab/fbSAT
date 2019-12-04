package ru.ifmo.fbsat.core.constraints

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.ScenarioTreeInterface
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffImply
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.implyIff
import ru.ifmo.fbsat.core.solver.implyIffAnd
import ru.ifmo.fbsat.core.solver.implyIffIte
import ru.ifmo.fbsat.core.solver.implyOr
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.ConsecutiveModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.basic.parallel.ParallelModularBasicVariables
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.complete.CompleteVariables
import ru.ifmo.fbsat.core.utils.algorithmChoice
import ru.ifmo.fbsat.core.utils.boolToSign
import ru.ifmo.fbsat.core.utils.exhaustive

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

fun Solver.declarePositiveParallelModularMappingConstraints(
    parallelModularBasicVariables: ParallelModularBasicVariables,
    isEncodeReverseImplication: Boolean
) {
    comment("Positive parallel modular mapping constraints")

    // Note: yet we only consider the SYNC-ALL mapping semantics

    with(parallelModularBasicVariables) {
        for (m in 1..M) with(modularBasicVariables[m]) {
            /* Constraints for the root */
            comment("Positive parallel modular mapping constraints: for root, module m = $m")
            declareMappingConstraintsForRoot(mapping = mapping)

            /* Constraints for active vertices */
            for (v in scenarioTree.activeVertices) {
                comment("Positive parallel modular mapping constraints: for active node v = $v, module m = $m")
                declareParallelModularMappingConstraintsForActiveNode(
                    m = m, v = v,
                    tree = scenarioTree,
                    C = C, Z = Z,
                    stateOutputEvent = stateOutputEvent,
                    stateAlgorithmTop = stateAlgorithmTop,
                    stateAlgorithmBot = stateAlgorithmBot,
                    actualTransitionFunction = actualTransitionFunction,
                    moduleControllingOutputVariable = moduleControllingOutputVariable,
                    mapping = mapping
                )
            }

            /* Constraints for passive vertices */
            for (v in scenarioTree.passiveVertices) {
                comment("Positive parallel modular mapping constraints: for passive node v = $v, module m = $m")
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
                comment("Mysterious reverse-implication: for module m = $m")
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
}

fun Solver.declarePositiveConsecutiveModularMappingConstraints(
    consecutiveModularBasicVariables: ConsecutiveModularBasicVariables,
    isEncodeReverseImplication: Boolean
) {
    comment("Positive consecutive modular mapping constraints")

    with(consecutiveModularBasicVariables) {
        for (m in 1..M) with(modularBasicVariables[m]) {
            /* Constraints for the root */
            comment("Positive consecutive modular mapping constraints: for root, module m = $m")
            declareMappingConstraintsForRoot(mapping = mapping)

            /* Constraints for active vertices */
            for (v in scenarioTree.activeVertices) {
                comment("Positive consecutive modular mapping constraints: for active node v = $v, module m = $m")
                declareConsecutiveModularMappingConstraintsForActiveNode(
                    m = m, v = v,
                    tree = scenarioTree,
                    C = C, Z = Z,
                    stateOutputEvent = stateOutputEvent,
                    actualTransitionFunction = actualTransitionFunction,
                    mapping = mapping,
                    modularComputedOutputValue = modularComputedOutputValue
                )
            }

            /* Constraints for passive vertices */
            for (v in scenarioTree.passiveVertices) {
                comment("Positive consecutive modular mapping constraints: for passive node v = $v, module m = $m")
                declareConsecutiveModularMappingConstraintsForPassiveNode(
                    m = m, v = v,
                    tree = scenarioTree,
                    C = C, O = O,
                    actualTransitionFunction = actualTransitionFunction,
                    mapping = mapping
                )
            }

            /* Additional constraints for module */

            // Nope, yet
        }

        // TODO: encode reverse-implication

        comment("Computed output value definition")
        // for root
        for (m in 1..M)
            for (z in 1..Z)
                clause(-modularComputedOutputValue[m][1, z])

        for (v in 2..V) {
            val p = scenarioTree.parent(v)

            // modularComputedOutputValue{1}[v,z] <=> stateAlgorithm{tov(tp(v),z)}[mapping[v],z]
            for (c in 1..C)
                for (z in 1..Z)
                    implyIff(
                        modularMapping[1][v, c],
                        modularComputedOutputValue[1][v, z],
                        when (scenarioTree.outputValue(p, z)) {
                            true -> modularStateAlgorithmTop[1][c, z]
                            false -> modularStateAlgorithmBot[1][c, z]
                        }.exhaustive
                    )

            // modularComputedOutputValue{m>1}[v,z] <=> stateAlgorithm{modularComputedOutputValue{m-1}[v,z]}[mapping[v],z]
            for (m in 2..M)
                for (c in 1..C)
                    for (z in 1..Z)
                        implyIffIte(
                            modularMapping[m][v, c],
                            modularComputedOutputValue[m][v, z],
                            modularComputedOutputValue[m - 1][v, z],
                            modularStateAlgorithmTop[m][c, z],
                            modularStateAlgorithmBot[m][c, z]
                        )
        }
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

private fun Solver.declareParallelModularMappingConstraintsForActiveNode(
    m: Int,
    v: Int,
    tree: ScenarioTreeInterface,
    C: Int,
    Z: Int,
    stateOutputEvent: IntMultiArray,
    stateAlgorithmTop: IntMultiArray,
    stateAlgorithmBot: IntMultiArray,
    actualTransitionFunction: IntMultiArray,
    moduleControllingOutputVariable: IntMultiArray,
    mapping: IntMultiArray
) {
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)
    val o = tree.outputEvent(v)

    comment("Parallel modular mapping definition for active node v = $v, module m = $m")
    // (mapping[v] = c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)] = c) & (stateOutputEvent[c] = toe(v)) & AND_{z}( (moduleControllingOutputVariable[z] = m) => (stateAlgorithm{tov(tp(v),z)}(c,z) = tov(v,z)) )
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
                            newVariable().also { aux ->
                                iffImply(
                                    aux,
                                    moduleControllingOutputVariable[z, m],
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
            )
}

private fun Solver.declareConsecutiveModularMappingConstraintsForActiveNode(
    m: Int,
    v: Int,
    tree: ScenarioTreeInterface,
    C: Int,
    Z: Int,
    stateOutputEvent: IntMultiArray,
    actualTransitionFunction: IntMultiArray,
    mapping: IntMultiArray,
    modularComputedOutputValue: MultiArray<IntMultiArray>
) {
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)
    val o = tree.outputEvent(v)

    comment("Consecutive modular mapping definition for active node v = $v, module m = $m")
    when (m) {
        1 -> {
            // (mapping[v] = c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)] = c)
            for (i in 1..C)
                for (j in 1..C)
                    implyIff(
                        mapping[p, i],
                        mapping[v, j],
                        actualTransitionFunction[i, e, u, j]
                    )

            // stateOutputEvent[mapping[v]] = CNF
            for (c in 1..C)
                imply(
                    mapping[v, c],
                    stateOutputEvent[c, 1]
                )
        }
        2 -> {
            // (mapping[v] = c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)] = c) & (stateOutputEvent[c] = toe(v)) & AND_{z}(modularComputedOutputValue{m}(v,z)}(c,z) = tov(v,z)
            for (i in 1..C)
                for (j in 1..C)
                    implyIffAnd(
                        mapping[p, i],
                        mapping[v, j],
                        sequence {
                            yield(actualTransitionFunction[i, 1, u, j]) // Note: e=REQ
                            yield(stateOutputEvent[j, o])
                            for (z in 1..Z)
                                yield(modularComputedOutputValue[m][v, z] * boolToSign(tree.outputValue(v, z)))
                        }
                    )
        }
        else -> error("meh")
    }.exhaustive
}

private fun Solver.declareConsecutiveModularMappingConstraintsForPassiveNode(
    m: Int,
    v: Int,
    tree: ScenarioTreeInterface,
    C: Int,
    O: Int,
    actualTransitionFunction: IntMultiArray,
    mapping: IntMultiArray
) {
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)

    comment("Consecutive modular mapping definition for passive node v = $v, module m = $m")
    when (m) {
        1 -> {
            // // (mapping[v] = c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)] = c)
            // for (i in 1..C)
            //     for (j in 1..C)
            //         implyIff(
            //             mapping[p, i],
            //             mapping[v, j],
            //             actualTransitionFunction[i, e, u, j]
            //         )

            comment("Mapping propagation for passive node v = $v, module m = $m")
            // mapping[v] = mapping[tp(v)]
            for (c in 1..C)
                imply(
                    mapping[p, c],
                    mapping[v, c]
                )

            comment("Constraining actualTransitionFunction for passive node v = $v, module m = $m")
            // actualTransition[mapping[tp(v)],tie(v),tin(v)] = 0
            for (c in 1..C)
                imply(
                    mapping[p, c],
                    actualTransitionFunction[c, e, u, C + 1]
                )
        }
        2 -> {
            comment("Mapping propagation for passive node v = $v, module m = $m")
            // mapping[v] = mapping[tp(v)]
            for (c in 1..C)
                imply(
                    mapping[p, c],
                    mapping[v, c]
                )

            comment("Constraining actualTransitionFunction for passive node v = $v, module m = $m")
            // actualTransition[mapping[tp(v)],tie(v),tin(v)] = 0
            for (c in 1..C)
                imply(
                    mapping[p, c],
                    actualTransitionFunction[c, 1, u, C + 1] // Note: e=REQ
                )
        }
        else -> error("meh")
    }.exhaustive
}
