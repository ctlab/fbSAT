@file:Suppress("LocalVariableName", "ReplaceRangeToWithUntil")

package ru.ifmo.fbsat.core.task

import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.exactlyOne
import ru.ifmo.fbsat.core.solver.iff
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.implyAnd
import ru.ifmo.fbsat.core.solver.implyIff
import ru.ifmo.fbsat.core.solver.implyIffAnd
import ru.ifmo.fbsat.core.solver.implyIffOr
import ru.ifmo.fbsat.core.solver.implyOr
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.multiarray.IntMultiArray

fun Solver.declareColorConstraints(isEncodeReverseImplication: Boolean = true) {
    val scenarioTree: ScenarioTree by context
    val C: Int by context
    val K: Int by context
    val V: Int by context
    val color: IntMultiArray by context
    val transition: IntMultiArray by context
    val actualTransition: IntMultiArray by context

    comment("1. Color constraints")

    comment("1.0. ONE(color)_{1..C}")
    for (v in 1..V)
        exactlyOne {
            for (c in 1..C)
                yield(color[v, c])
        }

    comment("1.1. Color of active vertices")
    // color[tp(v), i] & color[v, j] => actual_transition[i,tie(v),tin(v),j]
    for (v in scenarioTree.activeVertices) {
        val p = scenarioTree.parent(v)
        val e = scenarioTree.inputEvent(v)
        val u = scenarioTree.inputNumber(v)
        for (i in 1..C)
            for (j in 1..C)
                clause(
                    -color[p, i],
                    -color[v, j],
                    actualTransition[i, e, u, j]
                )
    }
    if (isEncodeReverseImplication) {
        // OR_k(transition[i,k,j]) <=> OR_{v|active}( color[tp(v), i] & color[v, j] )
        for (i in 1..C)
            for (j in 1..C) {
                val lhsAux = newVariable()
                iffOr(lhsAux, sequence {
                    for (k in 1..K)
                        yield(transition[i, k, j])
                })

                val rhsAux = newVariable()
                iffOr(rhsAux, sequence {
                    for (v in scenarioTree.activeVertices) {
                        val p = scenarioTree.parent(v)
                        val aux = newVariable()
                        iffAnd(aux, color[p, i], color[v, j])
                        yield(aux)
                    }
                })

                imply(lhsAux, rhsAux)
            }
    }
    comment("1.2. Color of passive vertices")
    // color[tp(v), c] => actual_transition[c,tie(v),tin(v),0]
    for (v in scenarioTree.passiveVertices) {
        val p = scenarioTree.parent(v)
        val e = scenarioTree.inputEvent(v)
        val u = scenarioTree.inputNumber(v)
        for (c in 1..C)
            imply(color[p, c], actualTransition[c, e, u, C + 1])
    }

    comment("1.3. Color propagation for passive vertices")
    // color[tp(v), c] => color[v, c]
    for (v in scenarioTree.passiveVertices) {
        val p = scenarioTree.parent(v)
        for (c in 1..C)
            imply(color[p, c], color[v, c])
    }

    comment("1.4. Root corresponds to start state")
    clause(color[1, 1])
}

fun Solver.declareTransitionConstraints() {
    val C: Int by context
    val K: Int by context
    val E: Int by context
    val U: Int by context
    val transition: IntMultiArray by context
    val actualTransition: IntMultiArray by context
    val inputEvent: IntMultiArray by context
    val firstFired: IntMultiArray by context

    comment("2. Transition constraints")

    comment("2.0a. ONE(transition)_{0..C}")
    for (i in 1..C)
        for (k in 1..K)
            exactlyOne {
                for (j in 1..(C + 1))
                    yield(transition[i, k, j])
            }

    comment("2.0b. ONE(actual_transition)_{0..C}")
    for (i in 1..C)
        for (e in 1..E)
            for (u in 1..U)
                exactlyOne {
                    for (j in 1..(C + 1))
                        yield(actualTransition[i, e, u, j])
                }

    comment("2.0c. ONE(input_event)_{0..E}")
    for (c in 1..C)
        for (k in 1..K)
            exactlyOne {
                for (e in 1..(E + 1))
                    yield(inputEvent[c, k, e])
            }

    comment("2.1. Active transition definition")
    // actual_transition[i,e,u,j] <=> OR_k( transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k] )
    for (i in 1..C)
        for (e in 1..E)
            for (u in 1..U)
                for (j in 1..C)
                    iffOr(actualTransition[i, e, u, j], sequence {
                        for (k in 1..K) {
                            // aux <=> transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k]
                            val aux = newVariable()
                            iffAnd(
                                aux,
                                transition[i, k, j],
                                inputEvent[i, k, e],
                                firstFired[i, u, k]
                            )
                            yield(aux)
                        }
                    })

    comment("2.2. Null-transitions are last")
    // transition[k, 0] => transition[k+1, 0]
    for (c in 1..C)
        for (k in 1 until K)
            imply(transition[c, k, C + 1], transition[c, k + 1, C + 1])

    comment("2.3. Only null-transitions have no input event")
    // transition[k, 0] <=> input_event[k, 0]
    for (c in 1..C)
        for (k in 1..K)
            iff(transition[c, k, C + 1], inputEvent[c, k, E + 1])

    comment("+2.4. Ad-hoc: no transition to the first state")
    for (c in 1..C)
        for (k in 1..K)
            clause(-transition[c, k, 1])
}

fun Solver.declareFiringConstraints() {
    val C: Int by context
    val K: Int by context
    val U: Int by context
    val rootValue: IntMultiArray by context
    val firstFired: IntMultiArray by context
    val notFired: IntMultiArray by context

    comment("3. Firing constraints")

    comment("3.0. ONE(first_fired)_{0..K}")
    for (c in 1..C)
        for (u in 1..U)
            exactlyOne {
                for (k in 1..(K + 1))
                    yield(firstFired[c, u, k])
            }

    comment("3.1. first_fired definition")
    // first_fired[k] <=> root_value[k] & not_fired[k-1]
    for (c in 1..C)
        for (u in 1..U) {
            iff(firstFired[c, u, 1], rootValue[c, 1, u])
            for (k in 2..K)
                iffAnd(firstFired[c, u, k], rootValue[c, k, u], notFired[c, u, k - 1])
        }

    comment("3.2. not_fired definition")
    // not_fired[k] <=> ~root_value[k] & not_fired[k-1]
    for (c in 1..C)
        for (u in 1..U) {
            iff(notFired[c, u, 1], -rootValue[c, 1, u])
            for (k in 2..K)
                iffAnd(notFired[c, u, k], -rootValue[c, k, u], notFired[c, u, k - 1])
        }

    comment("3.3. Propagation of not-not_fired (maybe redundant)")
    // ~not_fired[k] => ~not_fired[k+1]
    for (c in 1..C)
        for (u in 1..U)
            for (k in 1 until K)
                imply(-notFired[c, u, k], -notFired[c, u, k + 1])

    comment("3.4. first_fired[0] <=> not_fired[K] (shortcut)")
    // first_fired[0] <=> not_fired[K]
    for (c in 1..C)
        for (u in 1..U)
            iff(firstFired[c, u, K + 1], notFired[c, u, K])
}

fun Solver.declareOutputEventConstraints() {
    val scenarioTree: ScenarioTree by context
    val C: Int by context
    val O: Int by context
    val color: IntMultiArray by context
    val outputEvent: IntMultiArray by context

    comment("4. Output event constraints")

    comment("4.0. ONE(output_event)_{1..O}")
    for (c in 1..C)
        exactlyOne {
            for (o in 1..O)
                yield(outputEvent[c, o])
        }

    comment("4.1. output_event definition")
    // color[v, c] => output_event[c, toe(v)]
    for (v in scenarioTree.activeVertices) {
        val o = scenarioTree.outputEvent(v)
        for (c in 1..C)
            imply(color[v, c], outputEvent[c, o])
    }

    comment("4.2. Start state does INITO (root's output event)")
    clause(outputEvent[1, scenarioTree.outputEvent(1)])
}

fun Solver.declareAlgorithmConstraints() {
    val scenarioTree: ScenarioTree by context
    val C: Int by context
    val Z: Int by context
    val color: IntMultiArray by context
    val algorithm0: IntMultiArray by context
    val algorithm1: IntMultiArray by context

    comment("5. Algorithm constraints")

    comment("5.1. Start state produces zero outputs")
    for (z in 1..Z) {
        clause(-algorithm0[1, z])
        clause(-algorithm1[1, z])
    }

    comment("5.2. Algorithms definition")
    for (v in scenarioTree.activeVertices) {
        val p = scenarioTree.parent(v)
        for (z in 1..Z) {
            val oldValue = scenarioTree.outputValue(p, z)
            val newValue = scenarioTree.outputValue(v, z)
            for (c in 1..C)
                imply(
                    color[v, c],
                    when (val values = oldValue to newValue) {
                        false to false -> -algorithm0[c, z]
                        false to true -> algorithm0[c, z]
                        true to false -> -algorithm1[c, z]
                        true to true -> algorithm1[c, z]
                        else -> error("Weird combination of values: $values")
                    }
                )
        }
    }
}

fun Solver.declareAutomatonBfsConstraints() {
    val C: Int by context
    val K: Int by context
    val falseVariable: Int by context
    val transition: IntMultiArray by context
    val bfsTransitionAutomaton = newArray(C, C)
    val bfsParentAutomaton = newArray(C, C) { (j, i) ->
        if (i < j) newVariable() else falseVariable
    }

    comment("6. Automaton BFS constraints")

    comment("6.1. bfs_t definition")
    // t[i, j] <=> OR_k( transition[i, k, j] )
    for (j in 1..C)
        for (i in 1..C)
            iffOr(bfsTransitionAutomaton[i, j], sequence {
                for (k in 1..K)
                    yield(transition[i, k, j])
            })

    comment("6.2. bfs_p definition")
    // p[j, i] <=> t[i, j] & AND_{k<i}( ~t[k, j] ) :: i<j
    for (j in 1..C)
        for (i in 1 until j)
            iffAnd(bfsParentAutomaton[j, i], sequence {
                yield(bfsTransitionAutomaton[i, j])
                for (k in 1 until i)
                    yield(-bfsTransitionAutomaton[k, j])
            })

    // // p_order[j, i] <=> p[j] >= i
    // val bfsParentAutomaton_order = newArray(C, C) { (j, i) ->
    //     if (i < j) newVariable() else falseVariable
    // }
    // // monotonicity
    // // (1) p_order[j, i] => p_order[j, i-1]
    // // for (j in 4..C)
    // //     for (i in 3 until j)
    // //         imply(bfsParentAutomaton_order[j, i], bfsParentAutomaton_order[j, i - 1])
    // // (2) ~p_order[j, i] => ~p_order[j, i+1]
    // for (j in 1..C)
    //     for (i in 1 until C)
    //         imply(-bfsParentAutomaton_order[j, i], -bfsParentAutomaton_order[j, i + 1])
    // // channel
    // for (j in 1..C) {
    //     // p[j,C] <=> p_order[j,C]
    //     iff(bfsParentAutomaton[j, C], bfsParentAutomaton_order[j, C])
    //     // p[j,i] <=> p_order[j,i] & ~p_order[j,i+1]
    //     for (i in 1 until C)
    //         iffAnd(bfsParentAutomaton[j, i], bfsParentAutomaton_order[j, i], -bfsParentAutomaton_order[j, i + 1])
    // }
    // // p[j] >= 1 :: j>1
    // for (j in 2..C)
    //     clause(bfsParentAutomaton_order[j, 1])
    // // p[j] < j  (same as)  ~p_order[j,j]  (Note: using monotonicity)
    // for (j in 1..C)
    //     clause(-bfsParentAutomaton_order[j, j])
    // // p[j, i] => p[j+1]>=i
    // for (j in 3 until C)
    //     for (i in 2 until j)
    //         imply(bfsParentAutomaton[j, i], bfsParentAutomaton_order[j + 1, i])

    // comment("6.3. ALO(p)")
    // // ALO_{i<j}( p[j,i] ) :: j>1
    // for (j in 2..C)
    //     atLeastOne {
    //         for (i in 1 until j)
    //             yield(bfsParentAutomaton[j, i])
    //     }
    comment("6.3+. EO(p)")
    // EO_{i<j}( p[j,i] ) :: j>1
    for (j in 2..C)
        exactlyOne {
            for (i in 1 until j)
                yield(bfsParentAutomaton[j, i])
        }

    comment("6.4. BFS(p)")
    // p[j, i] => ~p[j+1, k] :: LB<=k<i<j<UB
    for (j in 3 until C)
        for (i in 2 until j)
            for (k in 1 until i)
                imply(bfsParentAutomaton[j, i], -bfsParentAutomaton[j + 1, k])
}

fun Solver.declareGuardBfsConstraints() {
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val falseVariable: Int by context
    val parent: IntMultiArray by context

    comment("66. Guard BFS constraints")

    for (c in 1..C)
        for (k in 1..K) {
            // p[j, i] <=> parent[j, i]
            val bfsParentGuard = newArray(P, P + 1) { (j, i) ->
                if (i < j) parent[c, k, j, i] else falseVariable
            }

            // // p_order[j, i] <=> p[j] >= i
            // val bfsParentGuard_order = newArray(P, P)
            // // monotonicity
            // // ~p_order[j, i] => ~p_order[j, i+1]
            // for (j in 1..P)
            //     for (i in 1 until P)
            //         imply(-bfsParentGuard_order[j, i], -bfsParentGuard_order[j, i + 1])
            // // channel
            // for (j in 1..P) {
            //     // p[j,0] <=> ~p[j,1]
            //     iffAnd(bfsParentGuard[j, P + 1], -bfsParentGuard_order[j, 1])
            //     // p[j,P] <=> p_order[j,P]
            //     iffAnd(bfsParentGuard[j, P], bfsParentGuard_order[j, P])
            //     // p[j,i] <=> p_order[j,i] & ~p_order[j,i+1]
            //     for (i in 1 until P) {
            //         iffAnd(bfsParentGuard[j, i], bfsParentGuard_order[j, i], -bfsParentGuard_order[j, i + 1])
            //     }
            // }
            // // p[j] < j  (same as)  ~p_order[j,j]  (Note: using monotonicity)
            // for (j in 1..P)
            //     clause(-bfsParentGuard_order[j, j])
            // p[j, i] => p[j+1]>=i | p[j+1]=0
            // for (j in 1 until P)
            //     for (i in 1..P)
            //     // implyOr(bfsParentGuard[j, i], bfsParentGuard_order[j + 1, i], -bfsParentGuard_order[j + 1, 1])
            //         imply(bfsParentGuard[j, i], bfsParentGuard_order[j + 1, i])

            // p[j] < j
            for (j in 1..P)
                for (i in j..P)
                    clause(-bfsParentGuard[j, i])

            // p[j, 0] => p[j+1, 0] :: j>1
            for (j in 2 until P)
                imply(bfsParentGuard[j, P + 1], bfsParentGuard[j + 1, P + 1])

            comment("66.4. BFS(p)")
            // p[j, i] => ~p[j+1, n] :: LB<=n<i<j<UB
            for (j in 3 until P)
                for (i in 2 until j)
                    for (n in 1 until i)
                        imply(bfsParentGuard[j, i], -bfsParentGuard[j + 1, n])
        }
}

fun Solver.declareNodeTypeConstraints() {
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val transition: IntMultiArray by context
    val nodeType: IntMultiArray by context

    comment("7. Nodetype constraints")

    comment("7.0. ONE(nodetype)_{all nodetypes}")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                exactlyOne {
                    for (nt in NodeType.values()) {
                        if (Globals.IS_FORBID_OR && nt == NodeType.OR) continue
                        yield(nodeType[c, k, p, nt.value])
                    }
                }

    comment("7.1. Only null-transitions have no guard")
    // transition[0] <=> nodetype[1, NONE]
    for (c in 1..C)
        for (k in 1..K)
            iff(transition[c, k, C + 1], nodeType[c, k, 1, NodeType.NONE.value])
}

fun Solver.declareParentAndChildrenConstraints() {
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val nodeType: IntMultiArray by context
    val parent: IntMultiArray by context
    val childLeft: IntMultiArray by context
    val childRight: IntMultiArray by context

    comment("8. Parent and children constraints")

    comment("8.0a. ONE(parent)_{0..P} :: parent[p] < p")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                exactlyOne {
                    for (par in 1..(P + 1))
                        yield(parent[c, k, p, par])
                }

    comment("8.0b. ONE(child_left)_{0..P} :: child_left[p] >= p+1")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                exactlyOne {
                    for (ch in 1..(P + 1))
                        yield(childLeft[c, k, p, ch])
                }

    comment("8.0c. ONE(child_right)_{0..P} :: child_right[p] >= p+2")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                exactlyOne {
                    for (ch in 1..(P + 1))
                        yield(childRight[c, k, p, ch])
                }

    comment("8.1. parent<->child relation")
    // parent[ch, p] <=> (child_left[p, ch] | child_right[p, ch])
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (ch in (p + 1)..P)
                    iffOr(
                        parent[c, k, ch, p],
                        childLeft[c, k, p, ch],
                        childRight[c, k, p, ch]
                    )

    comment("8.2. Only typed nodes (except root) have a parent")
    // ~nodetype[p, NONE] <=> ~parent[p, 0] :: p>1
    for (c in 1..C)
        for (k in 1..K)
            for (p in 2..P)
                iff(
                    -nodeType[c, k, p, NodeType.NONE.value],
                    -parent[c, k, p, P + 1]
                )

    comment("8.3. Root has no parent")
    for (c in 1..C)
        for (k in 1..K)
            clause(parent[c, k, 1, P + 1])
}

fun Solver.declareNoneTypeNodesConstraints() {
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val U: Int by context
    val nodeType: IntMultiArray by context
    val parent: IntMultiArray by context
    val childLeft: IntMultiArray by context
    val childRight: IntMultiArray by context
    val nodeValue: IntMultiArray by context
    val childValueLeft: IntMultiArray by context
    val childValueRight: IntMultiArray by context

    comment("9. None-type nodes constraints")

    comment("9.1. None-type nodes have largest numbers")
    // nodetype[p, NONE] => nodetype[p+1, NONE]
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1 until P)
                imply(
                    nodeType[c, k, p, NodeType.NONE.value],
                    nodeType[c, k, p + 1, NodeType.NONE.value]
                )

    comment("9.2. None-type nodes have no parent and no children")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                implyAnd(
                    nodeType[c, k, p, NodeType.NONE.value],
                    parent[c, k, p, P + 1],
                    childLeft[c, k, p, P + 1],
                    childRight[c, k, p, P + 1]
                )

    comment("9.3. None-type nodes have False value and False child-values")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (u in 1..U)
                    implyAnd(
                        nodeType[c, k, p, NodeType.NONE.value],
                        -nodeValue[c, k, p, u],
                        -childValueLeft[c, k, p, u],
                        -childValueRight[c, k, p, u]
                    )
}

fun Solver.declareTerminalsConstraints() {
    val scenarioTree: ScenarioTree by context
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val U: Int by context
    val X: Int by context
    val nodeType: IntMultiArray by context
    val terminal: IntMultiArray by context
    val childLeft: IntMultiArray by context
    val childRight: IntMultiArray by context
    val nodeValue: IntMultiArray by context
    val childValueLeft: IntMultiArray by context
    val childValueRight: IntMultiArray by context

    comment("10. Terminals constraints")

    comment("10.0. ONE(terminal)_{0..X}")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                exactlyOne {
                    for (x in 1..(X + 1))
                        yield(terminal[c, k, p, x])
                }

    comment("10.1. Only terminals have associated terminal variables")
    // nodetype[p, TERMINAL] <=> -terminal[p, 0]
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                iff(
                    nodeType[c, k, p, NodeType.TERMINAL.value],
                    -terminal[c, k, p, X + 1]
                )

    comment("10.2. Terminals have no children")
    // nodetype[p, TERMINAL] => child_left[p, 0] & child_right[p, 0]
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                implyAnd(
                    nodeType[c, k, p, NodeType.TERMINAL.value],
                    childLeft[c, k, p, P + 1],
                    childRight[c, k, p, P + 1]
                )

    comment("10.3. Terminal: child_value_left and child_value_right are False")
    // nodetype[p, TERMINAL] => AND_u( ~child_value_left[p, u] & ~child_value_right[p, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (u in 1..U)
                    implyAnd(
                        nodeType[c, k, p, NodeType.TERMINAL.value],
                        -childValueLeft[c, k, p, u],
                        -childValueRight[c, k, p, u]
                    )

    comment("10.4. Terminals have value from associated input variable")
    // terminal[p, x] => AND_u( value[p, u] <=> u[x] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (u in 1..U)
                    for (x in 1..X)
                        when (val char = scenarioTree.uniqueInputs[u - 1][x - 1]) {
                            '1' -> imply(terminal[c, k, p, x], nodeValue[c, k, p, u])
                            '0' -> imply(terminal[c, k, p, x], -nodeValue[c, k, p, u])
                            else -> error("Character $char for u = $u, x = $x is neither '1' nor '0'")
                        }

    if (Globals.IS_ENCODE_TERMINALS_ORDER) {
        comment("10.5. Terminals order")
        // terminal[p, x] => AND_{p'<p, x'>=x}( ~terminal[r_, x_] )
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..P)
                    for (x in 1..X)
                        implyAnd(terminal[c, k, p, x], sequence {
                            for (p_ in 1 until p)
                                for (x_ in x..X)
                                    yield(-terminal[c, k, p_, x_])
                        })
    }
}

fun Solver.declareAndOrNodesConstraints() {
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val U: Int by context
    val nodeType: IntMultiArray by context
    val parent: IntMultiArray by context
    val childLeft: IntMultiArray by context
    val childRight: IntMultiArray by context
    val nodeValue: IntMultiArray by context
    val childValueLeft: IntMultiArray by context
    val childValueRight: IntMultiArray by context

    comment("11. AND/OR nodes constraints")

    comment("11.0. AND/OR nodes cannot have numbers P-1 or P")
    for (c in 1..C)
        for (k in 1..K) {
            if (P >= 1) {
                clause(-nodeType[c, k, P, NodeType.AND.value])
                if (!Globals.IS_FORBID_OR) clause(-nodeType[c, k, P, NodeType.OR.value])
            }
            if (P >= 2) {
                clause(-nodeType[c, k, P - 1, NodeType.AND.value])
                if (!Globals.IS_FORBID_OR) clause(-nodeType[c, k, P - 1, NodeType.OR.value])
            }
        }

    comment("11.1. AND/OR: left child has greater number")
    // nodetype[p, AND/OR] => OR_{ch in (p+1)..(P-1)}( child_left[p, ch] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (nt in sequenceOf(NodeType.AND, NodeType.OR)) {
                    if (Globals.IS_FORBID_OR && nt == NodeType.OR) continue
                    implyOr(nodeType[c, k, p, nt.value], sequence {
                        for (ch in (p + 1)..(P - 1))
                            yield(childLeft[c, k, p, ch])
                    })
                }

    comment("11.2. AND/OR: right child is adjacent (+1) to left")
    // nodetype[p, AND/OR] & child_left[p, ch] => child_right[p, ch+1]
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (ch in (p + 1)..(P - 1))
                    for (nt in sequenceOf(NodeType.AND, NodeType.OR)) {
                        if (Globals.IS_FORBID_OR && nt == NodeType.OR) continue
                        clause(
                            -nodeType[c, k, p, nt.value],
                            -childLeft[c, k, p, ch],
                            childRight[c, k, p, ch + 1]
                        )
                    }

    comment("11.3. AND/OR: children`s parents")
    // nodetype[p, AND/OR] & child_left[p, ch] => parent[ch, p] & parent[ch+1, p]
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (ch in (p + 1)..(P - 1))
                    for (nt in sequenceOf(NodeType.AND, NodeType.OR)) {
                        if (Globals.IS_FORBID_OR && nt == NodeType.OR) continue
                        val x1 = nodeType[c, k, p, nt.value]
                        val x2 = childLeft[c, k, p, ch]
                        val x3 = parent[c, k, ch, p]
                        val x4 = parent[c, k, ch + 1, p]
                        clause(-x1, -x2, x3)
                        clause(-x1, -x2, x4)
                    }

    comment("11.4a. AND/OR: child_value_left is a value of left child")
    // nodetype[p, AND/OR] & child_left[p, ch] => AND_u( child_value_left[p,u] <=> value[ch, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (ch in (p + 1)..(P - 1))
                    for (u in 1..U)
                        for (nt in sequenceOf(NodeType.AND, NodeType.OR)) {
                            if (Globals.IS_FORBID_OR && nt == NodeType.OR) continue
                            val x1 = nodeType[c, k, p, nt.value]
                            val x2 = childLeft[c, k, p, ch]
                            val x3 = childValueLeft[c, k, p, u]
                            val x4 = nodeValue[c, k, ch, u]
                            clause(-x1, -x2, -x3, x4)
                            clause(-x1, -x2, x3, -x4)
                        }

    comment("11.4b. AND/OR: child_value_right is a value of right child")
    // nodetype[p, AND/OR] & child_right[p, ch] => AND_u( child_value_right[p,u] <=> value[ch, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (ch in (p + 2)..P)
                    for (u in 1..U)
                        for (nt in sequenceOf(NodeType.AND, NodeType.OR)) {
                            if (Globals.IS_FORBID_OR && nt == NodeType.OR) continue
                            val x1 = nodeType[c, k, p, nt.value]
                            val x2 = childRight[c, k, p, ch]
                            val x3 = childValueRight[c, k, p, u]
                            val x4 = nodeValue[c, k, ch, u]
                            clause(-x1, -x2, -x3, x4)
                            clause(-x1, -x2, x3, -x4)
                        }

    comment("11.5a. AND: value is calculated as a conjunction of children")
    // nodetype[p, AND] => AND_u(value[p, u] <=> (child_value_left[p, u] & child_value_right[p, u]))
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (u in 1..U)
                    implyIffAnd(
                        nodeType[c, k, p, NodeType.AND.value],
                        nodeValue[c, k, p, u],
                        childValueLeft[c, k, p, u],
                        childValueRight[c, k, p, u]
                    )

    if (!Globals.IS_FORBID_OR) {
        comment("11.5b. OR: value is calculated as a disjunction of children")
        // nodetype[p, OR] => AND_u( value[p, u] <=> (child_value_left[p, u] | child_value_right[p, u]) )
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..(P - 2))
                    for (u in 1..U)
                        implyIffOr(
                            nodeType[c, k, p, NodeType.OR.value],
                            nodeValue[c, k, p, u],
                            childValueLeft[c, k, p, u],
                            childValueRight[c, k, p, u]
                        )
    }
}

fun Solver.declareNotNodesConstraints() {
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val U: Int by context
    val nodeType: IntMultiArray by context
    val parent: IntMultiArray by context
    val childLeft: IntMultiArray by context
    val childRight: IntMultiArray by context
    val nodeValue: IntMultiArray by context
    val childValueLeft: IntMultiArray by context
    val childValueRight: IntMultiArray by context

    comment("12. NOT nodes constraints")

    comment("12.0. NOT nodes cannot have number P")
    for (c in 1..C)
        for (k in 1..K)
            clause(-nodeType[c, k, P, NodeType.NOT.value])

    comment("12.1. NOT: left child has greater number")
    // nodetype[p, NOT] => OR_{ch in (p+1)..P}( child_left[p, ch] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                implyOr(nodeType[c, k, p, NodeType.NOT.value], sequence {
                    for (ch in (p + 1)..P)
                        yield(childLeft[c, k, p, ch])
                })

    comment("12.2. NOT: child`s parents")
    // nodetype[p, NOT] & child_left[p, ch] => parent[ch, p]
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                for (ch in (p + 1)..P)
                    clause(
                        -nodeType[c, k, p, NodeType.NOT.value],
                        -childLeft[c, k, p, ch],
                        parent[c, k, ch, p]
                    )

    comment("12.3. NOT: no right child")
    // nodetype[p, NOT] => child_right[p, 0]
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                imply(
                    nodeType[c, k, p, NodeType.NOT.value],
                    childRight[c, k, p, P + 1]
                )

    comment("12.4a. NOT: child_value_left is a value of left child")
    // nodetype[p, NOT] & child_left[p, ch] => AND_u( child_value_left[p, u] <=> value[ch, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                for (ch in (p + 1)..P)
                    for (u in 1..U) {
                        val x1 = nodeType[c, k, p, NodeType.NOT.value]
                        val x2 = childLeft[c, k, p, ch]
                        val x3 = childValueLeft[c, k, p, u]
                        val x4 = nodeValue[c, k, ch, u]
                        clause(-x1, -x2, -x3, x4)
                        clause(-x1, -x2, x3, -x4)
                    }

    comment("12.4b. NOT: child_value_right is False")
    // nodetype[p, NOT] => AND_u( ~child_value_right[p, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                for (u in 1..U)
                    imply(
                        nodeType[c, k, p, NodeType.NOT.value],
                        -childValueRight[c, k, p, u]
                    )

    comment("12.5. NOT: value is calculated as a negation of child")
    // nodetype[p, NOT] => AND_u( value[p, u] <=> ~child_value_left[p, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                for (u in 1..U)
                    implyIff(
                        nodeType[c, k, p, NodeType.NOT.value],
                        nodeValue[c, k, p, u],
                        -childValueLeft[c, k, p, u]
                    )
}

fun Solver.declareTransitionsOrderConstraints() {
    val C: Int by context
    val K: Int by context
    val transition: IntMultiArray by context

    comment("+++. Transitions order constraints")

    // transition[i,k,j] => AND_{k'<k, j'>j}( ~transition[i,k',j'] )
    for (i in 1..C)
        for (k in 2..K)
            for (k_ in 1 until k)
                for (j in 1..(C - 1))
                    for (j_ in (j + 1)..C)
                        imply(transition[i, k, j], -transition[i, k_, j_])

    // transition[i,k,j] => AND_{k'<k}( OR_{j'<=j}( transition[i,k',j'] ) )
    for (i in 1..C)
        for (k in 2..K)
            for (k_ in 1 until k)
                for (j in 1..C)
                    clause {
                        yield(-transition[i, k, j])
                        for (j_ in 1..j)
                            yield(transition[i, k_, j_])
                    }
}

fun Solver.declareNegativeColorConstraints() {
    val negativeScenarioTree: NegativeScenarioTree by context
    val C: Int by context
    val Z: Int by context
    val negV: Int by context
    val newNegVs: IntRange by context
    val newNegVsActive: List<Int> by context
    val newNegVsPassive: List<Int> by context
    val outputEvent: IntMultiArray by context
    val algorithm0: IntMultiArray by context
    val algorithm1: IntMultiArray by context
    val satisfaction: IntMultiArray by context
    val negActualTransition: IntMultiArray by context
    val forbiddenLoops: MutableSet<Pair<Int, Int>> by context

    comment("Neg.1. Satisfaction (color-like) constrains")

    comment("Neg.1.0. ONE(satisfaction)_{0..C}")
    for (v in newNegVs)
        exactlyOne {
            for (c in 1..(C + 1))
                yield(satisfaction[v, c])
        }

    comment("Neg.1.1. Satisfaction of active vertices")
    // satisfaction[tp(v), i] => (satisfaction[v, j] <=> ...
    // ... <=> actual_transition[i,tie(v),tin(v),j] & output_event[j, toe(v)] & output_values[j, tov(v)])
    for (v in newNegVsActive) {
        val p = negativeScenarioTree.parent(v)
        val e = negativeScenarioTree.inputEvent(v)
        val u = negativeScenarioTree.inputNumber(v)
        val o = negativeScenarioTree.outputEvent(v)

        for (j in 1..C) {
            // aux = output_values[j, tov(v)]
            // aux <=> AND_{z in tov(v), z' in tov(p)}( z ~~> z' )
            val aux = newVariable()
            iffAnd(aux, sequence {
                for (z in 1..Z) {
                    val oldValue = negativeScenarioTree.outputValue(p, z)
                    val newValue = negativeScenarioTree.outputValue(v, z)

                    yield(
                        when (val values = oldValue to newValue) {
                            false to false -> -algorithm0[j, z]
                            false to true -> algorithm0[j, z]
                            true to false -> -algorithm1[j, z]
                            true to true -> algorithm1[j, z]
                            else -> error("Weird combination of old/new values: $values")
                        }
                    )
                }
            })

            for (i in 1..C)
                implyIffAnd(
                    satisfaction[p, i],
                    satisfaction[v, j],
                    negActualTransition[i, e, u, j],
                    outputEvent[j, o],
                    aux
                )
        }
    }

    comment("Neg.1.1+. Non-satisfaction of active vertices (redundant)")
    // satisfaction[tp(v), c] & actual_transition[c,tie(v),tin(v),0] => satisfaction[v, 0]
    @Suppress("UNREACHABLE_CODE")
    for (v in newNegVsActive) {
        break
        val p = negativeScenarioTree.parent(v)
        val e = negativeScenarioTree.inputEvent(v)
        val u = negativeScenarioTree.inputNumber(v)
        for (c in 1..C)
            clause(
                -satisfaction[p, c],
                -negActualTransition[c, e, u, C + 1],
                satisfaction[v, C + 1]
            )
    }

    comment("Neg.1.2. Satisfaction of passive vertices")
    // satisfaction[tp(v), c] & actual_transition[c,tie(v),tin(v),0] => satisfaction[v, c]
    for (v in newNegVsPassive) {
        val p = negativeScenarioTree.parent(v)
        val e = negativeScenarioTree.inputEvent(v)
        val u = negativeScenarioTree.inputNumber(v)
        for (c in 1..C)
            clause(
                -satisfaction[p, c],
                -negActualTransition[c, e, u, C + 1],
                satisfaction[v, c]
            )
    }

    comment("Neg.1.2+. Non-satisfaction of passive vertices")
    // satisfaction[tp(v), c] & ~actual_transition[c,tie(v),tin(v),0] => satisfaction[v, 0]
    for (v in newNegVsPassive) {
        val p = negativeScenarioTree.parent(v)
        val e = negativeScenarioTree.inputEvent(v)
        val u = negativeScenarioTree.inputNumber(v)
        for (i in 1..C)
            for (j in 1..C)
                clause(
                    -satisfaction[p, i],
                    negActualTransition[i, e, u, C + 1],
                    satisfaction[v, C + 1]
                )
    }

    // comment("Neg.1.3. Passive vertex may be satisfied only as its parent")
    // // satisfaction[tp(v), c] => satisfaction[v, c] | satisfaction[v, 0]
    // for (v in passiveVs) {
    //     val p = negativeScenarioTree.parent(v)
    //     for (c in 1..C)
    //         clause(
    //             -satisfaction[p, c],
    //             satisfaction[v, c],
    //             satisfaction[v, C + 1]
    //         )
    // }

    comment("Neg.1.4. Propagation of non-satisfaction")
    // satisfaction[tp(v), 0] => satisfaction[v, 0]
    for (v in newNegVs.filter { it != 1 }) {
        val p = negativeScenarioTree.parent(v)
        imply(satisfaction[p, C + 1], satisfaction[v, C + 1])
    }

    comment("Neg.1.5. Forbid loops")
    // satisfaction[v, c] => ~satisfaction[loop(v), c]
    for (v in 1..negV)
        for (l in negativeScenarioTree.loopBacks(v))
            if (forbiddenLoops.add(v to l))
                for (c in 1..C)
                    imply(satisfaction[v, c], -satisfaction[l, c])

    comment("Neg.1.6. Root is satisfied by start state")
    if (1 in newNegVs)
        clause(satisfaction[1, 1])
}

fun Solver.declareNegativeTransitionConstraints() {
    val C: Int by context
    val K: Int by context
    val E: Int by context
    val newOnlyNegUs: List<Int> by context
    val transition: IntMultiArray by context
    val inputEvent: IntMultiArray by context
    val negActualTransition: IntMultiArray by context
    val negFirstFired: IntMultiArray by context

    comment("Neg.2. Transition constraints")

    comment("Neg.2.0b. ONE(actual_transition)_{0..C}")
    for (i in 1..C)
        for (e in 1..E)
            for (u in newOnlyNegUs)
                exactlyOne {
                    for (j in 1..(C + 1))
                        yield(negActualTransition[i, e, u, j])
                }

    comment("Neg.2.1. Active transition definition")
    // actual_transition[i,e,u,j] <=> OR_k( transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k] )
    for (i in 1..C)
        for (e in 1..E)
            for (u in newOnlyNegUs)
                for (j in 1..C)
                    iffOr(negActualTransition[i, e, u, j], sequence {
                        for (k in 1..K) {
                            // aux <=> transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k]
                            val aux = newVariable()
                            iffAnd(
                                aux,
                                transition[i, k, j],
                                inputEvent[i, k, e],
                                negFirstFired[i, u, k]
                            )
                            yield(aux)
                        }
                    })
}

fun Solver.declareNegativeFiringConstraints() {
    val C: Int by context
    val K: Int by context
    val newOnlyNegUs: List<Int> by context
    val negRootValue: IntMultiArray by context
    val negFirstFired: IntMultiArray by context
    val negNotFired: IntMultiArray by context

    comment("Neg.3. Firing constraints")

    comment("Neg.3.0. ONE(first_fired)_{0..K}")
    for (c in 1..C)
        for (u in newOnlyNegUs)
            exactlyOne {
                for (k in 1..(K + 1))
                    yield(negFirstFired[c, u, k])
            }

    comment("Neg.3.1. first_fired definition")
    // first_fired[k] <=> root_value[k] & not_fired[k-1]
    for (c in 1..C)
        for (u in newOnlyNegUs) {
            iff(negFirstFired[c, u, 1], negRootValue[c, 1, u])
            for (k in 2..K)
                iffAnd(negFirstFired[c, u, k], negRootValue[c, k, u], negNotFired[c, u, k - 1])
        }

    comment("Neg.3.2. not_fired definition")
    // not_fired[k] <=> ~root_value[k] & not_fired[k-1]
    for (c in 1..C)
        for (u in newOnlyNegUs) {
            iff(negNotFired[c, u, 1], -negRootValue[c, 1, u])
            for (k in 2..K)
                iffAnd(negNotFired[c, u, k], -negRootValue[c, k, u], negNotFired[c, u, k - 1])
        }

    comment("Neg.3.3. Propagation of not-not_fired (maybe redundant)")
    // ~not_fired[k] => ~not_fired[k+1]
    for (c in 1..C)
        for (u in newOnlyNegUs)
            for (k in 1..(K - 1))
                imply(-negNotFired[c, u, k], -negNotFired[c, u, k + 1])

    comment("Neg.3.4. first_fired[0] <=> not_fired[K] (shortcut)")
    // first_fired[0] <=> not_fired[K]
    for (c in 1..C)
        for (u in newOnlyNegUs)
            iff(negFirstFired[c, u, K + 1], negNotFired[c, u, K])
}

fun Solver.declareNegativeGuardConstraints() {
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val X: Int by context
    val negUIs: List<String> by context
    val newOnlyNegUs: List<Int> by context
    val nodeType: IntMultiArray by context
    val terminal: IntMultiArray by context
    val childLeft: IntMultiArray by context
    val childRight: IntMultiArray by context
    val negNodeValue: IntMultiArray by context
    val negChildValueLeft: IntMultiArray by context
    val negChildValueRight: IntMultiArray by context

    comment("Neg. Guard constraints re-definition for CE unique inputs")

    comment("Neg.9.3. None-type nodes have False value and child_values")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (u in newOnlyNegUs)
                    implyAnd(
                        nodeType[c, k, p, NodeType.NONE.value],
                        -negNodeValue[c, k, p, u],
                        -negChildValueLeft[c, k, p, u],
                        -negChildValueRight[c, k, p, u]
                    )

    comment("Neg.10.3. Terminal: child_value_left and child_value_right are False")
    // nodetype[p, TERMINAL] => AND_u( ~child_value_left[p, u] & ~child_value_right[p, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (u in newOnlyNegUs)
                    implyAnd(
                        nodeType[c, k, p, NodeType.TERMINAL.value],
                        -negChildValueLeft[c, k, p, u],
                        -negChildValueRight[c, k, p, u]
                    )

    comment("Neg.10.4. Terminals have value from associated input variable")
    // terminal[p, x] => AND_u( value[p, u] <=> u[x] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (u in newOnlyNegUs)
                    for (x in 1..X)
                        when (val char = negUIs[u - 1][x - 1]) {
                            '1' -> imply(terminal[c, k, p, x], negNodeValue[c, k, p, u])
                            '0' -> imply(terminal[c, k, p, x], -negNodeValue[c, k, p, u])
                            else -> error("Character $char for u = $u, x = $x is neither '1' nor '0'")
                        }

    comment("Neg.11.4a. AND/OR: child_value_left is a value of left child")
    // nodetype[p, AND/OR] & child_left[p, ch] => AND_u( child_value_left[p,u] <=> value[ch, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (ch in (p + 1)..(P - 1))
                    for (u in newOnlyNegUs)
                        for (nt in sequenceOf(NodeType.AND, NodeType.OR)) {
                            if (Globals.IS_FORBID_OR && nt == NodeType.OR) continue
                            val x1 = nodeType[c, k, p, nt.value]
                            val x2 = childLeft[c, k, p, ch]
                            val x3 = negChildValueLeft[c, k, p, u]
                            val x4 = negNodeValue[c, k, ch, u]
                            clause(-x1, -x2, -x3, x4)
                            clause(-x1, -x2, x3, -x4)
                        }

    comment("Neg.11.4b. AND/OR: child_value_right is a value of right child")
    // nodetype[p, AND/OR] & child_right[p, ch] => AND_u( child_value_right[p,u] <=> value[ch, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (ch in (p + 2)..P)
                    for (u in newOnlyNegUs)
                        for (nt in sequenceOf(NodeType.AND, NodeType.OR)) {
                            if (Globals.IS_FORBID_OR && nt == NodeType.OR) continue
                            val x1 = nodeType[c, k, p, nt.value]
                            val x2 = childRight[c, k, p, ch]
                            val x3 = negChildValueRight[c, k, p, u]
                            val x4 = negNodeValue[c, k, ch, u]
                            clause(-x1, -x2, -x3, x4)
                            clause(-x1, -x2, x3, -x4)
                        }

    comment("Neg.11.5a. AND: value is calculated as a conjunction of children")
    // nodetype[p, AND] => AND_u(value[p, u] <=> (child_value_left[p, u] & child_value_right[p, u]))
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (u in newOnlyNegUs)
                    implyIffAnd(
                        nodeType[c, k, p, NodeType.AND.value],
                        negNodeValue[c, k, p, u],
                        negChildValueLeft[c, k, p, u],
                        negChildValueRight[c, k, p, u]
                    )

    if (!Globals.IS_FORBID_OR) {
        comment("Neg.11.5b. OR: value is calculated as a disjunction of children")
        // nodetype[p, OR] => AND_u( value[p, u] <=> (child_value_left[p, u] | child_value_right[p, u]) )
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..(P - 2))
                    for (u in newOnlyNegUs)
                        implyIffOr(
                            nodeType[c, k, p, NodeType.OR.value],
                            negNodeValue[c, k, p, u],
                            negChildValueLeft[c, k, p, u],
                            negChildValueRight[c, k, p, u]
                        )
    }

    comment("Neg.12.4a. NOT: child_value_left is a value of left child")
    // nodetype[p, NOT] & child_left[p, ch] => AND_u( child_value_left[p, u] <=> value[ch, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                for (ch in (p + 1)..P)
                    for (u in newOnlyNegUs) {
                        val x1 = nodeType[c, k, p, NodeType.NOT.value]
                        val x2 = childLeft[c, k, p, ch]
                        val x3 = negChildValueLeft[c, k, p, u]
                        val x4 = negNodeValue[c, k, ch, u]
                        clause(-x1, -x2, -x3, x4)
                        clause(-x1, -x2, x3, -x4)
                    }

    comment("Neg.12.4b. NOT: child_value_right is False")
    // nodetype[p, NOT] => AND_u( ~child_value_right[p, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                for (u in newOnlyNegUs)
                    imply(
                        nodeType[c, k, p, NodeType.NOT.value],
                        -negChildValueRight[c, k, p, u]
                    )

    comment("Neg.12.5. NOT: value is calculated as a negation of child")
    // nodetype[p, NOT] => AND_u( value[p, u] <=> ~child_value_left[p, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                for (u in newOnlyNegUs)
                    implyIff(
                        nodeType[c, k, p, NodeType.NOT.value],
                        negNodeValue[c, k, p, u],
                        -negChildValueLeft[c, k, p, u]
                    )
}
