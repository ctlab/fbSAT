@file:Suppress("LocalVariableName")

package ru.ifmo.fbsat.core.constraints

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.eq
import com.github.lipen.satlib.core.neq
import com.github.lipen.satlib.core.sign
import com.github.lipen.satlib.op.atLeastOne
import com.github.lipen.satlib.op.atMostOne
import com.github.lipen.satlib.op.iff
import com.github.lipen.satlib.op.iffAnd
import com.github.lipen.satlib.op.iffImply
import com.github.lipen.satlib.op.iffOr
import com.github.lipen.satlib.op.imply
import com.github.lipen.satlib.op.implyIff
import com.github.lipen.satlib.op.implyIffAnd
import com.github.lipen.satlib.op.implyIffIte
import com.github.lipen.satlib.op.implyImply
import com.github.lipen.satlib.op.implyImplyImply
import com.github.lipen.satlib.op.implyImplyOr
import com.github.lipen.satlib.op.implyOr
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.autoneg
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.forEachModularContext
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.Pins
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.algorithmChoice
import ru.ifmo.fbsat.core.utils.exhaustive
import ru.ifmo.fbsat.core.utils.withIndex

private val logger = MyLogger {}

fun Solver.declarePositiveMappingConstraints(
    isEncodeReverseImplication: Boolean,
) {
    comment("Positive mapping constraints")
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]

    /* Constraints for the root */
    comment("Positive mapping constraints: for root")
    declareMappingConstraintsForRoot(isPositive = true)

    /* Constraints for active vertices */
    comment("Positive mapping constraints: for active nodes")
    for (v in scenarioTree.activeVertices) {
        comment("Positive mapping constraints: for active node v = $v")
        declareMappingConstraintsForActiveNode(v = v, isPositive = true)
    }

    /* Constraints for passive vertices */
    comment("Positive mapping constraints: for passive nodes")
    for (v in scenarioTree.passiveVertices) {
        comment("Positive mapping constraints: for passive node v = $v")
        declareMappingConstraintsForPassiveNode(v = v, isPositive = true)
    }

    /* Additional constraints */

    run {
        comment("Adhoc mapping to WAIT state")
        val elementData: Map<ScenarioElement, Any> = context["elementData"]
        val mapping: IntVarArray = context["mapping"]
        for (node in scenarioTree.nodes.drop(1)) {
            @Suppress("UNCHECKED_CAST")
            val data = elementData.getValue(node.element) as Map<String, String>
            val state = data.getValue("state")
            when (state) {
                // "GO_WAIT" -> {
                //     logger.debug("Encoding mapping for GO_WAIT state: mapping[${node.id}] = 2")
                //     clause(mapping[node.id] eq 2)
                // }
                "WAIT" -> {
                    logger.debug("Encoding mapping for WAIT state: mapping[${node.id}] = 3")
                    clause(mapping[node.id] eq 3)
                }
                else -> {
                    // Do nothing
                }
            }.exhaustive
        }
    }

    if (isEncodeReverseImplication) {
        val C: Int = context["C"]
        val K: Int = context["K"]
        val transitionDestination: IntVarArray = context["transitionDestination"]
        val mapping: IntVarArray = context["mapping"]

        comment("Mysterious reverse-implication")
        // OR_k(transitionDestination[i,k,j]) => OR_{v|active}( mapping[tp(v),i] & mapping[v,j] )
        for (i in 1..C)
            for (j in 1..C) {
                val lhsAux = newLiteral()
                iffOr(lhsAux) {
                    for (k in 1..K)
                        yield(transitionDestination[i, k] eq j)
                }

                val rhsAux = newLiteral()
                iffOr(rhsAux) {
                    for (v in scenarioTree.activeVertices) {
                        val p = scenarioTree.parent(v)
                        val aux = newLiteral()
                        iffAnd(aux, mapping[p] eq i, mapping[v] eq j)
                        yield(aux)
                    }
                }

                imply(lhsAux, rhsAux)

                // Adhoc: other way around!
                imply(rhsAux, lhsAux)
            }
    }
}

fun Solver.declareNegativeMappingConstraints(
    Vs: Iterable<Int>,
    isForbidLoops: Boolean = true,
) {
    comment("Negative mapping constraints")
    val negativeScenarioTree: NegativeScenarioTree = context["negativeScenarioTree"]

    /* Constraints for the root */
    // Skip constraints for the root if they are already defined
    if (1 in Vs) {
        comment("Negative mapping constraints: for root")
        declareMappingConstraintsForRoot(isPositive = false)
    }

    /* Constraints for active vertices */
    comment("Negative mapping constraints: for active nodes")
    for (v in Vs.intersect(negativeScenarioTree.activeVertices)) {
        // comment("Negative mapping constraints: for active node v = $v")
        declareMappingConstraintsForActiveNode(v = v, isPositive = false)
    }

    /* Constraints for passive vertices */
    comment("Negative mapping constraints: for passive nodes")
    for (v in Vs.intersect(negativeScenarioTree.passiveVertices)) {
        // comment("Negative mapping constraints: for passive node v = $v")
        declareMappingConstraintsForPassiveNode(v = v, isPositive = false)
    }

    /* Additional constraints */
    comment("Additional negative mapping constraints")
    val negMapping: IntVarArray = context["negMapping"]

    comment("Non-satisfaction propagation")
    // (negMapping[tp(v)] = 0) => (negMapping[v] = 0)
    for (v in Vs.filter { it != 1 }) {
        val p = negativeScenarioTree.parent(v)
        imply(
            negMapping[p] eq 0,
            negMapping[v] eq 0
        )
    }

    if (isForbidLoops) {
        val C: Int = context["C"]
        val negV: Int = context["negV"]
        val forbiddenLoops: MutableSet<Pair<Int, Int>> = context["forbiddenLoops"]

        comment("Forbid loops")
        // (negMapping[v]=c) => AND_{l in loopBacks(v)}(negMapping[l] != c)
        for (v in 1..negV)
            for (l in negativeScenarioTree.loopBacks(v))
                if (forbiddenLoops.add(v to l))
                    for (c in 1..C)
                        imply(
                            negMapping[v] eq c,
                            negMapping[l] neq c
                        )
    }
}

fun Solver.declarePositiveParallelModularMappingConstraints(
    isEncodeReverseImplication: Boolean,
) {
    comment("Positive parallel modular mapping constraints")
    // Note: yet we only consider the SYNC-ALL mapping semantics
    val moduleControllingOutputVariable: IntVarArray = context["moduleControllingOutputVariable"]

    forEachModularContext { m ->
        val scenarioTree: PositiveScenarioTree = context["scenarioTree"]

        /* Constraints for the root */
        comment("Positive parallel modular mapping constraints: for root, module m = $m")
        declareMappingConstraintsForRoot(isPositive = true)

        /* Constraints for active vertices */
        for (v in scenarioTree.activeVertices) {
            comment("Positive parallel modular mapping constraints: for active node v = $v, module m = $m")
            declareParallelModularMappingConstraintsForActiveNode(
                m = m, v = v,
                moduleControllingOutputVariable = moduleControllingOutputVariable
            )
        }

        /* Constraints for passive vertices */
        for (v in scenarioTree.passiveVertices) {
            comment("Positive parallel modular mapping constraints: for passive node v = $v, module m = $m")
            declareMappingConstraintsForPassiveNode(v = v, isPositive = true)
        }

        /* Additional constraints */

        if (isEncodeReverseImplication) {
            comment("Mysterious reverse-implication: for module m = $m")
            // OR_k(transitionDestination[i,k,j]) => OR_{v|active}( mapping[tp(v),i] & mapping[v,j] )
            val C: Int = context["C"]
            val K: Int = context["K"]
            val transitionDestination: IntVarArray = context["transitionDestination"]
            val mapping: IntVarArray = context["mapping"]
            for (i in 1..C)
                for (j in 1..C) {
                    val lhsAux = newLiteral()
                    iffOr(lhsAux) {
                        for (k in 1..K)
                            yield(transitionDestination[i, k] eq j)
                    }

                    val rhsAux = newLiteral()
                    iffOr(rhsAux) {
                        for (v in scenarioTree.activeVertices) {
                            val p = scenarioTree.parent(v)
                            val aux = newLiteral()
                            iffAnd(aux, mapping[p] eq i, mapping[v] eq j)
                            yield(aux)
                        }
                    }

                    imply(lhsAux, rhsAux)
                }
        }
    }
}

fun Solver.declarePositiveConsecutiveModularMappingConstraints(
    isEncodeReverseImplication: Boolean,
) {
    comment("Positive consecutive modular mapping constraints")
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val V: Int = context["V"]
    val M: Int = context["M"]
    val Z: Int = context["Z"]
    val modularComputedOutputValue: MultiArray<BoolVarArray> = context["modularComputedOutputValue"]

    forEachModularContext { m ->
        /* Constraints for the root */
        comment("Positive consecutive modular mapping constraints: for root, module m = $m")
        declareMappingConstraintsForRoot(isPositive = true)

        /* Constraints for active vertices */
        for (v in scenarioTree.activeVertices) {
            comment("Positive consecutive modular mapping constraints: for active node v = $v, module m = $m")
            declareConsecutiveModularMappingConstraintsForActiveNode(
                m = m, v = v, M = M,
                modularComputedOutputValue = modularComputedOutputValue,
            )
        }

        /* Constraints for passive vertices */
        for (v in scenarioTree.passiveVertices) {
            comment("Positive consecutive modular mapping constraints: for passive node v = $v, module m = $m")
            declareConsecutiveModularMappingConstraintsForPassiveNode(m = m, v = v)
        }

        /* Additional constraints for module */

        // Nope, yet
    }

    // TODO: encode reverse-implication
    if (isEncodeReverseImplication) {
        logger.warn("Reverse-implication encoding is not implemented yet")
    }

    comment("Computed output value definition")
    // for root
    for (m in 1..M)
        for (z in 1..Z)
            clause(-modularComputedOutputValue[m][1, z])

    for (v in 2..V) {
        val p = scenarioTree.parent(v)

        forEachModularContext { m ->
            val C: Int = context["C"]
            // val Z: Int = context["Z"]
            val mapping: IntVarArray = context["mapping"]
            val stateAlgorithmBot: BoolVarArray = context["stateAlgorithmBot"]
            val stateAlgorithmTop: BoolVarArray = context["stateAlgorithmTop"]

            if (m == 1) {
                // modularComputedOutputValue{m=1}[v,z] <=> stateAlgorithm{tov(tp(v),z)}[mapping[v],z]
                for (c in 1..C)
                    for (z in 1..Z)
                        implyIff(
                            mapping[v] eq c,
                            modularComputedOutputValue[1][v, z],
                            when (scenarioTree.outputValue(p, z)) {
                                true -> stateAlgorithmTop[c, z]
                                false -> stateAlgorithmBot[c, z]
                            }.exhaustive
                        )
            } else {
                // modularComputedOutputValue{m>1}[v,z] <=> stateAlgorithm{modularComputedOutputValue{m-1}[v,z]}[mapping[v],z]
                for (c in 1..C)
                    for (z in 1..Z)
                        implyIffIte(
                            mapping[v] eq c,
                            modularComputedOutputValue[m][v, z],
                            modularComputedOutputValue[m - 1][v, z],
                            stateAlgorithmTop[c, z],
                            stateAlgorithmBot[c, z]
                        )
            }
        }
    }
}

fun Solver.declarePositiveArbitraryModularMappingConstraints(
    isEncodeReverseImplication: Boolean,
) {
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val M: Int = context["M"]
    val V: Int = context["V"]
    val E: Int = context["E"]
    val O: Int = context["O"]
    val X: Int = context["X"]
    val Z: Int = context["Z"]
    val inboundVarPinParent: IntVarArray = context["inboundVarPinParent"]
    // val outboundVarPinParent: IntVarArray = context["outboundVarPinParent"]
    val modularInputIndex: MultiArray<IntVarArray> = context["modularInputIndex"]
    val inboundVarPinComputedValue: BoolVarArray = context["inboundVarPinComputedValue"]
    val outboundVarPinComputedValue: BoolVarArray = context["outboundVarPinComputedValue"]

    comment("Positive arbitrary modular mapping constraints")
    forEachModularContext { m ->
        /* Constraints for the root */
        comment("Positive arbitrary modular mapping constraints: for root, module m = $m")
        declareMappingConstraintsForRoot(isPositive = true)

        /* Constraints for active vertices */
        for (v in scenarioTree.activeVertices) {
            comment("Positive arbitrary modular mapping constraints: for active node v = $v, module m = $m")
            declareArbitraryModularMappingConstraintsForActiveNode(
                m = m, v = v,
                M = M,
                inputIndex = modularInputIndex[m],
                inboundVarPinComputedValue = inboundVarPinComputedValue,
                outboundVarPinComputedValue = outboundVarPinComputedValue
            )
        }

        /* Constraints for passive vertices */
        for (v in scenarioTree.passiveVertices) {
            comment("Positive arbitrary modular mapping constraints: for passive node v = $v, module m = $m")
            declareArbitraryModularMappingConstraintsForPassiveNode(
                m = m, v = v,
                inputIndex = modularInputIndex[m],
                outboundVarPinComputedValue = outboundVarPinComputedValue
            )
        }

        /* Additional constraints for module */

        // Nope, yet
    }

    // TODO: encode reverse-implication
    if (isEncodeReverseImplication) {
        logger.warn("Reverse-implication encoding is not implemented yet")
    }

    comment("Additional arbitrary modular mapping constraints")
    with(Pins(M = M, X = X, Z = Z, E = E, O = O)) {
        comment("Pin value propagation from parent")
        for (pin in allInboundVarPins)
            for (parent in inboundVarPinParent[pin].domain - 0)
                for (v in 1..V)
                    implyIff(
                        inboundVarPinParent[pin] eq parent,
                        inboundVarPinComputedValue[v, pin],
                        outboundVarPinComputedValue[v, parent]
                    )

        comment("Initially (at v = 1), all outbound pins have false values")
        for (pin in allOutboundVarPins)
            clause(-outboundVarPinComputedValue[1, pin])

        comment("External outbound pins (input variable) have values from input values in the tree")
        for ((x, pin) in externalOutboundVarPins.withIndex(start = 1))
            for (v in 2..V)
                clause(outboundVarPinComputedValue[v, pin] sign scenarioTree.inputValue(v, x))

        comment("If pin does not have a parent, then it always has false value")
        for (pin in allInboundVarPins)
            for (v in 1..V)
                imply(
                    inboundVarPinParent[pin] eq 0,
                    -inboundVarPinComputedValue[v, pin]
                )

        comment("External outbound pins (input variables) must be connected to something")
        for (parent in externalOutboundVarPins)
            atLeastOne {
                for (pin in allInboundVarPins)
                    yield(inboundVarPinParent[pin] eq parent)
            }

        comment("External inbound pins (output variables) must be connected to something")
        for (pin in externalInboundVarPins)
            clause(inboundVarPinParent[pin] neq 0)

        comment("Module output pins cannot have two external output pins as children")
        for (m in 1..M)
            for (pin in modularOutboundVarPins[m])
                atMostOne {
                    for (extPin in externalInboundVarPins)
                        yield(inboundVarPinParent[extPin] eq pin)
                }
    }
}

fun Solver.declareDistributedPositiveMappingConstraints_modular(
    modularIsEncodeReverseImplication: MultiArray<Boolean>,
) {
    // comment("Distributed positive mapping constraints")
    // with(distributedBasicVariables) {
    //     for (m in 1..M) {
    //         comment("Distributed positive mapping constraints: module m = $m")
    //         declarePositiveMappingConstraints(
    //             basicVariables = modularBasicVariables[m],
    //             isEncodeReverseImplication = modularIsEncodeReverseImplication[m]
    //         )
    //     }
    // }
    TODO()
}

fun Solver.declareDistributedPositiveMappingConstraints_compound(
    modularIsEncodeReverseImplication: MultiArray<Boolean>,
) {
    comment("Distributed [COMPOUND] positive mapping constraints")
    forEachModularContext { m ->
        comment("Distributed [COMPOUND] positive mapping constraints: for module m = $m")
        val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
        val C: Int = context["C"]
        val mapping: IntVarArray = context["mapping"]
        val stateUsed: BoolVarArray = context["stateUsed"]

        /* Constraints for the root */
        comment("Positive mapping constraints: for root")
        declareMappingConstraintsForRoot(isPositive = true)

        /* Constraints for active vertices */
        for (v in scenarioTree.activeVertices) {
            comment("Positive mapping constraints: for active node v = $v")
            declareMappingConstraintsForActiveNode(v = v, isPositive = true)
        }

        /* Constraints for passive vertices */
        for (v in scenarioTree.passiveVertices) {
            comment("Positive mapping constraints: for passive node v = $v")
            declareMappingConstraintsForPassiveNode(v = v, isPositive = true)
        }

        /* Constraints for all vertices */
        for (v in 1..scenarioTree.size) {
            for (c in 1..C) {
                imply(
                    -stateUsed[c],
                    mapping[v] neq c
                )
            }
        }
    }
}

private fun Solver.declareParallelModularMappingConstraintsForActiveNode(
    m: Int,
    v: Int,
    moduleControllingOutputVariable: IntVarArray,
) {
    val tree: PositiveScenarioTree = context["tree"]
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)
    val o = tree.outputEvent(v)

    val C: Int = context["C"]
    val Z: Int = context["Z"]
    val stateOutputEvent: IntVarArray = context["stateOutputEvent"]
    val stateAlgorithmTop: BoolVarArray = context["stateAlgorithmTop"]
    val stateAlgorithmBot: BoolVarArray = context["stateAlgorithmBot"]
    val actualTransitionFunction: IntVarArray = context["actualTransitionFunction"]
    val mapping: IntVarArray = context["mapping"]

    comment("Parallel modular mapping definition for active node v = $v, module m = $m")
    // (mapping[v]=c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)]=c) & (stateOutputEvent[c]=toe(v)) & AND_{z}( (moduleControllingOutputVariable[z]=m) => (stateAlgorithm{tov(tp(v),z)}(c,z) = tov(v,z)) )
    for (i in 1..C)
        for (j in 1..C)
            implyIffAnd(
                mapping[p] eq i,
                mapping[v] eq j
            ) {
                yield(actualTransitionFunction[i, e, u] eq j)
                yield(stateOutputEvent[j] eq o)
                for (z in 1..Z)
                    yield(
                        newLiteral().also { aux ->
                            iffImply(
                                aux,
                                moduleControllingOutputVariable[z] eq m,
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
}

private fun Solver.declareConsecutiveModularMappingConstraintsForActiveNode(
    m: Int,
    v: Int,
    M: Int,
    modularComputedOutputValue: MultiArray<BoolVarArray>,
) {
    val tree: PositiveScenarioTree = context["tree"]
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)
    val o = tree.outputEvent(v)

    val C: Int = context["C"]
    val Z: Int = context["Z"]
    val actualTransitionFunction: IntVarArray = context["actualTransitionFunction"]
    val stateOutputEvent: IntVarArray = context["stateOutputEvent"]
    val mapping: IntVarArray = context["mapping"]

    comment("Consecutive modular mapping definition for active node v = $v, module m = $m")
    when (m) {
        1 -> {
            // (mapping[v]=c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)]=c)
            for (i in 1..C)
                for (j in 1..C)
                    implyIff(
                        mapping[p] eq i,
                        mapping[v] eq j,
                        actualTransitionFunction[i, e, u] eq j
                    )

            // stateOutputEvent[mapping[v]]=CNF
            for (c in 1..C)
                imply(
                    mapping[v] eq c,
                    stateOutputEvent[c] eq 1
                )
        }
        M -> {
            // (mapping[v]=c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)]=c) & (stateOutputEvent[c]=toe(v)) & AND_{z}(modularComputedOutputValue{m}(v,z)}(c,z) = tov(v,z)
            for (i in 1..C)
                for (j in 1..C)
                    implyIffAnd(
                        mapping[p] eq i,
                        mapping[v] eq j
                    ) {
                        yield(actualTransitionFunction[i, 1, u] eq j) // Note: e=REQ
                        yield(stateOutputEvent[j] eq o)
                        for (z in 1..Z)
                            yield(modularComputedOutputValue[m][v, z] sign tree.outputValue(v, z))
                    }
        }
        else -> {
            // (mapping[v]=c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)]=c)
            for (i in 1..C)
                for (j in 1..C)
                    implyIff(
                        mapping[p] eq i,
                        mapping[v] eq j,
                        actualTransitionFunction[i, 1, u] eq j // Note: e=REQ
                    )

            // stateOutputEvent[mapping[v]]=CNF
            for (c in 1..C)
                imply(
                    mapping[v] eq c,
                    stateOutputEvent[c] eq 1
                )
        }
    }.exhaustive
}

private fun Solver.declareConsecutiveModularMappingConstraintsForPassiveNode(
    m: Int,
    v: Int,
) {
    val tree: PositiveScenarioTree = context["tree"]
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)

    val C: Int = context["C"]
    val actualTransitionFunction: IntVarArray = context["actualTransitionFunction"]
    val mapping: IntVarArray = context["mapping"]

    comment("Consecutive modular mapping definition for passive node v = $v, module m = $m")
    // FIXME: should this place be empty?

    comment("Mapping propagation for passive node v = $v, module m = $m")
    // mapping[v] = mapping[tp(v)]
    for (c in 1..C)
        imply(
            mapping[p] eq c,
            mapping[v] eq c
        )

    if (m == 1) {
        comment("Constraining actualTransitionFunction for passive node v = $v, module m = $m")
        // actualTransition[mapping[tp(v)],tie(v),tin(v)]=0
        for (c in 1..C)
            imply(
                mapping[p] eq c,
                actualTransitionFunction[c, e, u] eq 0
            )
    } else {
        comment("Constraining actualTransitionFunction for passive node v = $v, module m = $m")
        // actualTransition[mapping[tp(v)],tie(v),tin(v)]=0
        for (c in 1..C)
            imply(
                mapping[p] eq c,
                actualTransitionFunction[c, 1, u] eq 0 // Note: e=REQ
            )
    }
}

private fun Solver.declareArbitraryModularMappingConstraintsForActiveNode(
    m: Int,
    v: Int,
    M: Int,
    inputIndex: IntVarArray,
    inboundVarPinComputedValue: BoolVarArray,
    outboundVarPinComputedValue: BoolVarArray,
) {
    val tree: PositiveScenarioTree = context["tree"]
    val p = tree.parent(v)

    val C: Int = context["C"]
    val X: Int = context["X"]
    val Z: Int = context["Z"]
    val U: Int = context["U"]
    val actualTransitionFunction: IntVarArray = context["actualTransitionFunction"]
    val stateAlgorithmBot: BoolVarArray = context["stateAlgorithmBot"]
    val stateAlgorithmTop: BoolVarArray = context["stateAlgorithmTop"]
    val mapping: IntVarArray = context["mapping"]

    // comment("Arbitrary modular mapping definition for active node v = $v, module m = $m")

    // (inputIndex[v]=u) => (mapping[tp(v)]=i) => (mapping[v]=j) <=> (actualTransition[i,u]=j))
    // Note: implyImplyImply vs implyImplyIff
    for (i in 1..C)
        for (j in 1..C)
            for (u in 1..U)
                implyImplyImply(
                    inputIndex[v] eq u,
                    mapping[p] eq i,
                    mapping[v] eq j,
                    actualTransitionFunction[i, 1, u] eq j
                )

    // redundant?
    // actualTransition[mapping[tp(v)], inputIndex[v]] != 0
    // (mapping[p]=c) => ((inputIndex[v]=u) => (actualTransition[c,u] != 0))
    // for (c in 1..C)
    //     for (u in 1..U)
    //         implyImply(
    //             mapping[p] eq c,
    //             inputIndex[v] eq u,
    //             actualTransitionFunction[c, u] neq 0
    //         )

    fun getOutboundVarPin(m: Int, z: Int): Int = (m - 1) * Z + z

    // (mapping[v]=c) => (pinComputedValue[v,z] <=> ITE(pinComputedValue[p,z], stateAlgorithmTop[c,z], stateAlgorithmBot[c,z]))
    for (c in 1..C)
        for (z in 1..Z) {
            val pin = getOutboundVarPin(m, z)
            implyIffIte(
                mapping[v] eq c,
                outboundVarPinComputedValue[v, pin],
                outboundVarPinComputedValue[p, pin],
                stateAlgorithmTop[c, z],
                stateAlgorithmBot[c, z]
            )
        }

    fun getExternalInboundVarPin(z: Int): Int = M * X + z

    // pinComputedValue{ext}[v,z] <=> tov(v,z)
    for (z in 1..Z) {
        val pin = getExternalInboundVarPin(z)
        clause(inboundVarPinComputedValue[v, pin] sign tree.outputValue(v, z))
    }
}

private fun Solver.declareArbitraryModularMappingConstraintsForPassiveNode(
    m: Int,
    v: Int,
    inputIndex: IntVarArray,
    outboundVarPinComputedValue: BoolVarArray,
) {
    val tree: PositiveScenarioTree = context["tree"]
    val p = tree.parent(v)

    val C: Int = context["C"]
    val Z: Int = context["Z"]
    val U: Int = context["U"]
    val actualTransitionFunction: IntVarArray = context["actualTransitionFunction"]
    val mapping: IntVarArray = context["mapping"]

    // (mapping[p] = q) => (mapping[v] = q)
    for (c in 1..C)
        imply(
            mapping[p] eq c,
            mapping[v] eq c
        )

    // (inputIndex[v] = u) => (actualTransition[mapping[tp(v)],u] = 0)
    for (c in 1..C)
        for (u in 1..U)
            implyImply(
                mapping[p] eq c,
                inputIndex[v] eq u,
                actualTransitionFunction[c, 1, u] eq 0
            )

    fun getOutboundVarPin(m: Int, z: Int): Int = (m - 1) * Z + z

    // pinComputedValue[v,z] <=> pinComputedValue[p,z]
    for (z in 1..Z) {
        val pin = getOutboundVarPin(m, z)
        iff(
            outboundVarPinComputedValue[v, pin],
            outboundVarPinComputedValue[p, pin]
        )
    }
}

private fun Solver.declareMappingConstraintsForRoot(
    isPositive: Boolean,
) {
    val mapping: IntVarArray = context.autoneg("mapping", isPositive)

    comment("Root maps to the initial state")
    clause(mapping[1] eq 1)
}

private fun Solver.declareMappingConstraintsForActiveNode(
    v: Int,
    isPositive: Boolean,
) {
    val tree: ScenarioTree<*, *> = context.autoneg("tree", isPositive)
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)
    val o = tree.outputEvent(v)

    if (e == 0) {
        // log.warn("Empty input event when declaring mapping constraints for the active node v = $v!")
        // return
        error("This is unexpected")
    }

    val C: Int = context["C"]
    val Z: Int = context["Z"]
    val stateOutputEvent: IntVarArray = context["stateOutputEvent"]
    val stateAlgorithmTop: BoolVarArray = context["stateAlgorithmTop"]
    val stateAlgorithmBot: BoolVarArray = context["stateAlgorithmBot"]
    val actualTransitionFunction: IntVarArray = context.autoneg("actualTransitionFunction", isPositive)
    val mapping: IntVarArray = context.autoneg("mapping", isPositive)

    if (isPositive) {
        comment("Positive mapping definition for active node v = $v")
        // (mapping[v] = c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)] = c)
        for (i in 1..C)
            for (j in 1..C)
                implyIff(
                    mapping[p] eq i,
                    mapping[v] eq j,
                    actualTransitionFunction[i, e, u] eq j
                )
        // (mapping[v] = c) => (stateOutputEvent[c] = toe(v))
        for (c in 1..C)
            imply(
                mapping[v] eq c,
                stateOutputEvent[c] eq o
            )
        // (mapping[v] = c) => AND_{z}(stateAlgorithm{tov(tp(v),z)}[c,z] = tov(v,z))
        for (c in 1..C)
            for (z in 1..Z)
                imply(
                    mapping[v] eq c,
                    algorithmChoice(
                        tree = tree,
                        v = v, c = c, z = z,
                        algorithmTop = stateAlgorithmTop,
                        algorithmBot = stateAlgorithmBot
                    )
                )
    } else {
        // comment("Negative mapping definition for active node v = $v")
        // // (mapping[v]=c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)]=c) & (stateOutputEvent[c]=toe(v)) & AND_{z}(stateAlgorithm{tov(tp(v),z)}[c,z] = tov(v,z))
        // for (i in 1..C)
        //     for (j in 1..C)
        //         implyIffAnd(
        //             mapping[p] eq i,
        //             mapping[v] eq j
        //         ) {
        //             yield(actualTransitionFunction[i, e, u] eq j)
        //             yield(stateOutputEvent[j] eq o)
        //             for (z in 1..Z)
        //                 yield(
        //                     algorithmChoice(
        //                         tree = tree,
        //                         v = v, c = j, z = z,
        //                         algorithmTop = stateAlgorithmTop,
        //                         algorithmBot = stateAlgorithmBot
        //                     )
        //                 )
        //         }

        comment("Negative mapping definition for active node v = $v")
        // (mapping[v] = c) => (actualTransition[mapping[tp(v)],tie(v),tin(v)] = c)
        for (i in 1..C)
            for (j in 1..C)
                implyImply(
                    mapping[p] eq i,
                    mapping[v] eq j,
                    actualTransitionFunction[i, e, u] eq j
                )
        // (mapping[v] = c) => (stateOutputEvent[c] = toe(v))
        for (c in 1..C)
            imply(
                mapping[v] eq c,
                stateOutputEvent[c] eq o
            )
        // (mapping[v] = c) => AND_{z}(stateAlgorithm{tov(tp(v),z)}(c,z) = tov(v,z))
        for (c in 1..C)
            for (z in 1..Z)
                imply(
                    mapping[v] eq c,
                    algorithmChoice(
                        tree = tree,
                        v = v, c = c, z = z,
                        algorithmTop = stateAlgorithmTop,
                        algorithmBot = stateAlgorithmBot
                    )
                )
        // (mapping[v] = 0) => (
        //   (actualTransition[mapping[tp(v)],tie(v),tin(v)] = 0)
        //    \/
        //   OR_{c} (
        //     (actualTransition[mapping[tp(v)],tie(v),tin(v)] = c)
        //      /\
        //     ((stateOutputEvent[c] != toe(v)) \/ OR_{z} (stateAlgorithm[c,z] != tov(v,z)))
        //   )
        // )
        for (i in 1..C)
            implyImplyOr(
                mapping[p] eq i,
                mapping[v] eq 0
            ) {
                yield(actualTransitionFunction[i, e, u] eq 0)
                for (j in 1..C) {
                    val internalAux = newLiteral()
                    iffOr(internalAux) {
                        yield(stateOutputEvent[j] neq o)
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

                    val aux = newLiteral()
                    iffAnd(
                        aux,
                        actualTransitionFunction[i, e, u] eq j,
                        internalAux
                    )
                    yield(aux)
                }
            }
        // (actualTransition[mapping[tp(v)],tie(v),tin(v)] = c) & (stateOutputEvent[c] = toe(v)) & AND_{z}(stateAlgorithm{tov(tp(v),z)}[c,z] = tov(v,z)) => (mapping[v] = c)
        for (i in 1..C)
            for (j in 1..C)
                clause {
                    yield(-(mapping[p] eq i))
                    yield(-(actualTransitionFunction[i, e, u] eq j))
                    yield(-(stateOutputEvent[j] eq o))
                    for (z in 1..Z)
                        yield(
                            -algorithmChoice(
                                tree = tree,
                                v = v, c = j, z = z,
                                algorithmTop = stateAlgorithmTop,
                                algorithmBot = stateAlgorithmBot
                            )
                        )
                    yield(mapping[v] eq j)
                }
    }
}

private fun Solver.declareMappingConstraintsForPassiveNode(
    v: Int,
    isPositive: Boolean,
) {
    val tree: ScenarioTree<*, *> = context.autoneg("tree", isPositive)
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)

    val C: Int = context["C"]
    val actualTransitionFunction: IntVarArray = context.autoneg("actualTransitionFunction", isPositive)
    val mapping: IntVarArray = context.autoneg("mapping", isPositive)

    if (e == 0) {
        // log.warn("Empty input event when declaring mapping constraints for the passive node v = $v!")

        for (c in 1..C) {
            imply(
                mapping[p] eq c,
                mapping[v] eq c
            )
        }

        return
    }

    if (isPositive) {
        comment("Positive mapping propagation for passive node v = $v")
        // mapping[v] = mapping[tp(v)]
        for (c in 1..C)
            imply(
                mapping[p] eq c,
                mapping[v] eq c
            )

        comment("Constraining actualTransitionFunction for passive node v = $v")
        // (mapping[tp(v)] = c) => (actualTransition[c,tie(v),tin(v)] = 0)
        for (c in 1..C)
            imply(
                mapping[p] eq c,
                actualTransitionFunction[c, e, u] eq 0
            )
    } else {
        comment("Negative mapping propagation for passive node v = $v")
        // (negMapping[tp(v)] = c) => (negMapping[v] = c) \/ (negMapping[v] = 0)
        for (c in 1..C)
            implyOr(
                mapping[p] eq c,
                mapping[v] eq c,
                mapping[v] eq 0
            )

        comment("Constraining negActualTransitionFunction for passive node v = $v")
        // (negMapping[tp(v)] = c) => ((negMapping[v] = c) <=> (negActualTransition[c,tie(v),tin(v)] = 0))
        for (c in 1..C)
            implyIff(
                mapping[p] eq c,
                mapping[v] eq c,
                actualTransitionFunction[c, e, u] eq 0
            )
        // (negMapping[tp(v)] = c) => ((negMapping[v] = 0) <=> (negActualTransition[c,tie(v),tin(v)] != 0))
        for (c in 1..C)
            implyIff(
                mapping[p] eq c,
                mapping[v] eq 0,
                actualTransitionFunction[c, e, u] neq 0
            )
    }
}
