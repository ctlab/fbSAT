package ru.ifmo.fbsat.core.constraints2

import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.scenario2.positive.ScenarioTree2
import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.implyImply
import ru.ifmo.fbsat.core.task.single.basic2.BasicVariables2
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.algorithmChoice2

fun Solver.declarePositiveMappingConstraints2(
    basicVariables: BasicVariables2,
    isEncodeReverseImplication: Boolean
) {
    comment("Positive mapping constraints")
    with(basicVariables) {
        /* Constraints for the root */
        comment("Positive mapping constraints: for root")
        declareMappingConstraintsForRoot2(mapping = mapping)

        /* Constraints for active vertices */
        for (v in scenarioTree.activeVertices) {
            comment("Positive mapping constraints: for active node v = $v")
            declareMappingConstraintsForActiveNode2(
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
            declareMappingConstraintsForPassiveNode2(
                v = v,
                tree = scenarioTree,
                C = C, O = O,
                stateOutputEvent = stateOutputEvent,
                actualTransitionFunction = actualTransitionFunction,
                mapping = mapping
            )
        }

        /* Additional constraints */

        if (isEncodeReverseImplication) {
            comment("Mysterious reverse-implication")
            // OR_k(transitionDestination[i,k,j]) => OR_{v|active}( mapping[tp(v),i] & mapping[v,j] )
            for (i in 1..C)
                for (j in 1..C) {
                    val lhsAux = newLiteral()
                    iffOr(lhsAux, sequence {
                        for (k in 1..K)
                            yield(transitionDestination[i, k] eq j)
                    })

                    val rhsAux = newLiteral()
                    iffOr(rhsAux, sequence {
                        for (v in scenarioTree.activeVertices) {
                            val p = scenarioTree.parent(v)
                            val aux = newLiteral()
                            iffAnd(aux, mapping[p] eq i, mapping[v] eq j)
                            yield(aux)
                        }
                    })

                    imply(lhsAux, rhsAux)

                    // Adhoc: other way around!
                    imply(rhsAux, lhsAux)
                }
        }
    }
}

private fun Solver.declareMappingConstraintsForRoot2(
    mapping: IntVarArray
) {
    comment("Root maps to the initial state")
    clause(mapping[1] eq 1)
}


private fun Solver.declareMappingConstraintsForActiveNode2(
    v: Int,
    tree: ScenarioTree2,
    C: Int,
    Z: Int,
    stateOutputEvent: IntVarArray,
    stateAlgorithmTop: BoolVarArray,
    stateAlgorithmBot: BoolVarArray,
    actualTransitionFunction: IntVarArray,
    mapping: IntVarArray
) {
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)
    // val o = tree.outputEvent(v)

    comment("Mapping definition for active node v = $v")
    // (mapping[v]=c) => (actualTransition[mapping[tp(v)],tie(v),tin(v)]=c) & (stateOutputEvent[c]=toe(v)) & AND_{z}(stateAlgorithm{tov(tp(v),z)}(c,z) = tov(v,z))
    for (i in 1..C)
        for (j in 1..C)
            implyImply(
                mapping[p] eq i,
                mapping[v] eq j,
                actualTransitionFunction[i, e, u] eq j
            )
    val outputActions = tree.nodes[v - 1].outputActions
    check(outputActions.isNotEmpty())
    check(outputActions.size <= Globals.OUTPUT_ACTIONS_MULTIPLICITY)
    for (c in 1..C) {
        for (a in 1..outputActions.size) {
            val outputAction = outputActions[a - 1]

            val o = tree.outputEvents.indexOf(outputAction.event) + 1
            imply(mapping[v] eq c, stateOutputEvent[c, a] eq o)

            for (z in 1..Z) {
                imply(
                    mapping[v] eq c,
                    algorithmChoice2(tree, v, c, z, a, stateAlgorithmTop, stateAlgorithmBot)
                )
            }
        }
        for (a in (outputActions.size + 1)..Globals.OUTPUT_ACTIONS_MULTIPLICITY) {
            imply(mapping[v] eq c, stateOutputEvent[c, a] eq 0)
        }
    }
}

private fun Solver.declareMappingConstraintsForPassiveNode2(
    v: Int,
    tree: ScenarioTree2,
    C: Int,
    O: Int,
    stateOutputEvent: IntVarArray,
    actualTransitionFunction: IntVarArray,
    mapping: IntVarArray
) {
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)

    comment("Mapping propagation for passive node v = $v")
    // mapping[v] = mapping[tp(v)]
    for (c in 1..C)
        imply(
            mapping[p] eq c,
            mapping[v] eq c
        )

    comment("Constraining actualTransitionFunction for passive node v = $v")
    // actualTransition[mapping[tp(v)],tie(v),tin(v)] = 0
    for (c in 1..C)
        imply(
            mapping[p] eq c,
            actualTransitionFunction[c, e, u] eq 0
        )
}
