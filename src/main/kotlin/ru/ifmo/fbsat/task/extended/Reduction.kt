// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++, C#, and Java: http://www.viva64.com

package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.automaton.NodeType
import ru.ifmo.fbsat.scenario.CounterExampleTree
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.solver.atMostOne
import ru.ifmo.fbsat.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.solver.declareTotalizer
import ru.ifmo.fbsat.solver.exactlyOne
import ru.ifmo.fbsat.solver.iff
import ru.ifmo.fbsat.solver.iffAnd
import ru.ifmo.fbsat.solver.iffOr
import ru.ifmo.fbsat.solver.imply
import ru.ifmo.fbsat.solver.implyIff
import ru.ifmo.fbsat.solver.implyIffAnd
import ru.ifmo.fbsat.solver.implyIffOr
import ru.ifmo.fbsat.utils.IntMultiArray

class Reduction(
    val scenarioTree: ScenarioTree,
    val C: Int,
    val K: Int,
    val P: Int,
    val color: IntMultiArray,
    val transition: IntMultiArray,
    val activeTransition: IntMultiArray,
    val inputEvent: IntMultiArray,
    val outputEvent: IntMultiArray,
    val algorithm0: IntMultiArray,
    val algorithm1: IntMultiArray,
    val nodeType: IntMultiArray,
    val terminal: IntMultiArray,
    val parent: IntMultiArray,
    val childLeft: IntMultiArray,
    val childRight: IntMultiArray,
    val nodeValue: IntMultiArray,
    val childValueLeft: IntMultiArray,
    val childValueRight: IntMultiArray,
    val firstFired: IntMultiArray,
    val notFired: IntMultiArray
)

class CEReduction(
    val ceTree: CounterExampleTree,
    val C: Int,
    val K: Int,
    val P: Int,
    val satisfaction: IntMultiArray,
    val activeTransition: IntMultiArray,
    val nodeValue: IntMultiArray,
    val childValueLeft: IntMultiArray,
    val childValueRight: IntMultiArray,
    val firstFired: IntMultiArray,
    val notFired: IntMultiArray
)

@Suppress("LocalVariableName")
fun Solver.declareBaseReductionExtended(scenarioTree: ScenarioTree, C: Int, K: Int, P: Int): Reduction {
    // Constants
    val V = scenarioTree.size
    val E = scenarioTree.inputEvents.size
    val O = scenarioTree.outputEvents.size
    val U = scenarioTree.uniqueInputs.size
    val X = scenarioTree.uniqueInputs.first().length
    val Z = scenarioTree.uniqueOutputs.first().length
    // Scenario tree variables
    val color = newArray(V, C)
    // Automaton variables
    val transition = newArray(C, K, C + 1)
    val activeTransition = newArray(C, C, E, U)
    val inputEvent = newArray(C, K, E + 1)
    val outputEvent = newArray(C, O)
    val algorithm0 = newArray(C, Z)
    val algorithm1 = newArray(C, Z)
    // Guards variables
    val nodeType = newArray(C, K, P, 5)
    val terminal = newArray(C, K, P, X + 1)
    val parent = newArray(C, K, P, P + 1)
    val childLeft = newArray(C, K, P, P + 1)
    val childRight = newArray(C, K, P, P + 1)
    val nodeValue = newArray(C, K, P, U)
    val childValueLeft = newArray(C, K, P, U)
    val childValueRight = newArray(C, K, P, U)
    val firstFired = newArray(C, U, K)
    val notFired = newArray(C, U, K)
    // BFS variables
    val bfs_transition = newArray(C, C)
    val bfs_parent = newArray(C, C)


    comment("1. Color constraints")
    comment("1.0. ONE(color)")
    for (v in 1..V)
        exactlyOne(1..C, color, v)

    comment("1.1. Color propagation for passive vertices")
    // color[tp(v), c] => color[v, c]
    for (v in scenarioTree.passiveVertices) {
        val p = scenarioTree.parent(v)
        for (c in 1..C)
            imply(color[p, c], color[v, c])
    }

    comment("1.2. Root corresponds to start state")
    clause(color[1, 1])


    comment("2. Transition constraints")
    comment("2.0a. ONE(transition)")
    for (c in 1..C)
        for (k in 1..K)
            exactlyOne(1..(C + 1), transition, c, k)

    comment("2.0b. ONE(input_event)")
    for (c in 1..C)
        for (k in 1..K)
            exactlyOne(1..(E + 1), inputEvent, c, k)

    comment("2.1. Active transition existence")
    // color[tp(v), i] & color[v, j] => active_transition[i,j,tie(v),tin(v)]
    for (v in scenarioTree.activeVertices) {
        val p = scenarioTree.parent(v)
        val e = scenarioTree.inputEvent(v)
        val u = scenarioTree.inputNumber(v)
        for (i in 1..C)
            for (j in 1..C)
                clause(
                    -color[p, i],
                    -color[v, j],
                    activeTransition[i, j, e, u]
                )
    }

    comment("2.2. Active transition definition")
    // active_transition[i,j,e,u] <=> OR_k( transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k] )
    for (i in 1..C)
        for (j in 1..C)
            for (e in 1..E)
                for (u in 1..U)
                    iffOr(activeTransition[i, j, e, u], sequence {
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

    comment("2.3. Null-transitions are last")
    // transition[k, 0] => transition[k+1, 0]
    for (c in 1..C)
        for (k in 1..(K - 1))
            imply(transition[c, k, C + 1], transition[c, k + 1, C + 1])

    comment("2.4. Only null-transitions have no input event")
    // transition[k, 0] <=> input_event[k, 0]
    for (c in 1..C)
        for (k in 1..K)
            iff(transition[c, k, C + 1], inputEvent[c, k, E + 1])


    comment("3. Firing constraints")
    comment("3.0. AMO(first_fired)")
    for (c in 1..C)
        for (u in 1..U)
            atMostOne(1..K, firstFired, c, u)

    comment("3.1. not_fired definition")
    // color[tp(v), c] => not_fired[c, tin(v), K]
    for (v in scenarioTree.passiveVertices) {
        val p = scenarioTree.parent(v)
        val u = scenarioTree.inputNumber(v)
        for (c in 1..C)
            imply(color[p, c], notFired[c, u, K])
    }

    comment("3.2. not_fired propagation")
    for (c in 1..C)
        for (u in 1..U) {
            // nf[k] => nf[k-1]
            for (k in 2..K)
                imply(notFired[c, u, k], notFired[c, u, k - 1])
            // ~nf[k] => ~nf[k+1]
            for (k in 1..(K - 1))
                imply(-notFired[c, u, k], -notFired[c, u, k + 1])
        }

    comment("3.3. first_fired and not_fired interaction")
    for (c in 1..C)
        for (u in 1..U) {
            // ~(ff & nf)
            for (k in 1..K)
                clause(-firstFired[c, u, k], -notFired[c, u, k])
            // ff[k] => nf[k-1]
            for (k in 2..K)
                imply(firstFired[c, u, k], notFired[c, u, k - 1])
        }

    comment("3.4. Value from not/first fired")
    for (c in 1..C)
        for (u in 1..U)
            for (k in 1..K) {
                // nf => ~root_value
                imply(notFired[c, u, k], -nodeValue[c, k, 1, u])
                // ff => root_value
                imply(firstFired[c, u, k], nodeValue[c, k, 1, u])
                // else => unconstrained
            }


    comment("4. Output event constraints")
    comment("4.0. ONE(output_event)")
    for (c in 1..C)
        exactlyOne(1..O, outputEvent, c)

    comment("4.1. output_event definition")
    // color[v, c] => output_event[c, toe(v)]
    for (v in scenarioTree.activeVertices) {
        val o = scenarioTree.outputEvent(v)
        for (c in 1..C)
            imply(color[v, c], outputEvent[c, o])
    }

    comment("4.2. Start state does INITO (root`s output event)")
    clause(outputEvent[1, scenarioTree.outputEvent(1)])


    comment("5. Algorithm constraints")
    comment("5.1. Start state does nothing")
    for (z in 1..Z) {
        clause(-algorithm0[1, z])
        clause(algorithm1[1, z])
    }

    comment("5.2. Algorithms definition")
    for (v in scenarioTree.activeVertices)
        for (z in 1..Z) {
            val oldValue = scenarioTree.outputValue(scenarioTree.parent(v), z)
            val newValue = scenarioTree.outputValue(v, z)
            for (c in 1..C)
                imply(
                    color[v, c],
                    when (val values = oldValue to newValue) {
                        false to false -> -algorithm0[c, z]
                        false to true -> algorithm0[c, z]
                        true to false -> -algorithm1[c, z]
                        true to true -> algorithm1[c, z]
                        else -> throw IllegalStateException("Weird combination of values: $values")
                    }
                )
        }


    comment("6. BFS constraints")
    comment("6.1. F_t")
    // t[i, j] <=> OR_k( transition[i,k,j] )
    for (i in 1..C)
        for (j in 1..C)
            iffOr(bfs_transition[i, j], sequence {
                for (k in 1..K)
                    yield(transition[i, k, j])
            })

    comment("6.2. F_p")
    // p[j, i] <=> t[i, j] & AND_{k<i}( ~t[k, j] )
    for (i in 1..C) {
        // to avoid ambiguous unused variable:
        for (j in 1..i)
            clause(-bfs_parent[j, i])

        for (j in (i + 1)..C)
            iffAnd(bfs_parent[j, i],
                sequence {
                    yield(bfs_transition[i, j])
                    for (k in 1..(i - 1))
                        yield(-bfs_transition[k, j])
                })
    }

    comment("6.3. F_ALO(p)")
    for (j in 2..C)
        clause(sequence {
            for (i in 1..(j - 1))
                yield(bfs_parent[j, i])
        })

    comment("6.4. F_BFS(p)")
    // p[j, i] => ~p[j+1, k]
    for (k in 1..C)
        for (i in (k + 1)..C)
            for (j in (i + 1)..(C - 1))
                imply(bfs_parent[j, i], -bfs_parent[j + 1, k])


    comment("7. Nodetype constraints")
    comment("7.0. ONE(nodetype)")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                exactlyOne(1..5, nodeType, c, k, p)


    comment("8. Parent and children constraints")
    comment("8.0a. ONE(parent)")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                exactlyOne(1..(P + 1), parent, c, k, p)

    comment("8.0b. ONE(child_left)")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                exactlyOne(1..(P + 1), childLeft, c, k, p)

    comment("8.0c. ONE(child_right)")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                exactlyOne(1..(P + 1), childRight, c, k, p)

    comment("8.1. parent<->child relation")
    // parent[ch, p] => (child_left[p, ch] | child_right[p, ch])
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                for (ch in (p + 1)..P)
                    clause(
                        -parent[c, k, ch, p],
                        childLeft[c, k, p, ch],
                        childRight[c, k, p, ch]
                    )

    comment("8.2. Typed nodes (except root) have parent with lesser number")
    // ~nodetype[p, NONE]  =>  OR_par( parent[p, par] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 2..P)
                clause(sequence {
                    yield(nodeType[c, k, p, NodeType.NONE.value])
                    for (par in 1..(p - 1))
                        yield(parent[c, k, p, par])
                })

    comment("8.3. Root has no parent")
    for (c in 1..C)
        for (k in 1..K)
            clause(parent[c, k, 1, P + 1])


    comment("9. None-type nodes constraints")
    comment("9.1. None-type nodes have largest numbers")
    // nodetype[p, NONE] => nodetype[p+1, NONE]
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                imply(
                    nodeType[c, k, p, NodeType.NONE.value],
                    nodeType[c, k, p + 1, NodeType.NONE.value]
                )

    comment("9.2. None-type nodes have no parent and no children")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P) {
                imply(
                    nodeType[c, k, p, NodeType.NONE.value],
                    parent[c, k, p, P + 1]
                )
                imply(
                    nodeType[c, k, p, NodeType.NONE.value],
                    childLeft[c, k, p, P + 1]
                )
                imply(
                    nodeType[c, k, p, NodeType.NONE.value],
                    childRight[c, k, p, P + 1]
                )
            }

    comment("9.3. None-type nodes have False value and child_values")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (u in 1..U) {
                    imply(
                        nodeType[c, k, p, NodeType.NONE.value],
                        -nodeValue[c, k, p, u]
                    )
                    imply(
                        nodeType[c, k, p, NodeType.NONE.value],
                        -childValueLeft[c, k, p, u]
                    )
                    imply(
                        nodeType[c, k, p, NodeType.NONE.value],
                        -childValueRight[c, k, p, u]
                    )
                }


    comment("10. Terminals constraints")
    comment("10.0. ONE(terminal)")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                exactlyOne(1..(X + 1), terminal, c, k, p)

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
            for (p in 1..P) {
                imply(
                    nodeType[c, k, p, NodeType.TERMINAL.value],
                    childLeft[c, k, p, P + 1]
                )
                imply(
                    nodeType[c, k, p, NodeType.TERMINAL.value],
                    childRight[c, k, p, P + 1]
                )
            }

    comment("10.3. Terminal: child_value_left and child_value_right are False")
    // nodetype[p, TERMINAL] => AND_u( ~child_value_left[p, u] & ~child_value_right[p, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (u in 1..U) {
                    imply(
                        nodeType[c, k, p, NodeType.TERMINAL.value],
                        -childValueLeft[c, k, p, u]
                    )
                    imply(
                        nodeType[c, k, p, NodeType.TERMINAL.value],
                        -childValueRight[c, k, p, u]
                    )
                }

    comment("10.4. Terminals have value from associated input variable")
    // terminal[p, x] => AND_u( value[p, u] <=> u[x] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (x in 1..X)
                    for (u in 1..U)
                        when (val char = scenarioTree.uniqueInputs[u - 1][x - 1]) {
                            '1' -> imply(terminal[c, k, p, x], nodeValue[c, k, p, u])
                            '0' -> imply(terminal[c, k, p, x], -nodeValue[c, k, p, u])
                            else -> throw IllegalStateException("Character $char is neither '1' nor '0'")
                        }


    comment("11. AND/OR nodes constraints")
    comment("11.0. AND/OR nodes cannot have numbers P-1 or P")
    for (c in 1..C)
        for (k in 1..K) {
            if (P >= 1) {
                clause(-nodeType[c, k, P, NodeType.AND.value])
                clause(-nodeType[c, k, P, NodeType.OR.value])
            }
            if (P >= 2) {
                clause(-nodeType[c, k, P - 1, NodeType.AND.value])
                clause(-nodeType[c, k, P - 1, NodeType.OR.value])
            }
        }

    comment("11.1. AND/OR: left child has greater number")
    // nodetype[p, AND/OR] => OR_{ch in (p+1)..(P-1)}( child_left[p, ch] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (nt in sequenceOf(NodeType.AND, NodeType.OR))
                    clause(sequence {
                        yield(-nodeType[c, k, p, nt.value])
                        for (ch in (p + 1)..(P - 1))
                            yield(childLeft[c, k, p, ch])
                    })

    comment("11.2. AND/OR: right child is adjacent (+1) to left")
    // nodetype[p, AND/OR] & child_left[p, ch] => child_right[p, ch+1]
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (ch in (p + 1)..(P - 1))
                    for (nt in sequenceOf(NodeType.AND, NodeType.OR))
                        clause(
                            -nodeType[c, k, p, nt.value],
                            -childLeft[c, k, p, ch],
                            childRight[c, k, p, ch + 1]
                        )

    comment("11.3. AND/OR: children`s parents")
    // nodetype[p, AND/OR] & child_left[p, ch] => parent[ch, p] & parent[ch+1, p]
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (ch in (p + 1)..(P - 1))
                    for (nt in sequenceOf(NodeType.AND, NodeType.OR)) {
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
                clause(sequence {
                    yield(-nodeType[c, k, p, NodeType.NOT.value])
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
                for (u in 1..U) {
                    val x1 = nodeType[c, k, p, NodeType.NOT.value]
                    val x2 = nodeValue[c, k, p, u]
                    val x3 = -childValueLeft[c, k, p, u]
                    clause(-x1, -x2, x3)
                    clause(-x1, x2, -x3)
                }


    comment("A. AD-HOCs")
    comment("A.1. (comb)")
    // t=0 <=> nodetype[1, NONE]
    for (c in 1..C)
        for (k in 1..K)
            iff(
                transition[c, k, C + 1],
                nodeType[c, k, 1, NodeType.NONE.value]
            )

    comment("A.2. (comb)")
    for (c in 1..C)
        for (k in 1..K)
            for (u in 1..U) {
                // (t!=0 & nf) => ~nodetype[1, NONE]
                clause(
                    transition[c, k, C + 1],
                    -notFired[c, u, k],
                    -nodeType[c, k, 1, NodeType.NONE.value]
                )
                // ff => ~nodetype[1, NONE]
                imply(
                    firstFired[c, u, k],
                    -nodeType[c, k, 1, NodeType.NONE.value]
                )
            }

    comment("A.3. Distinct transitions")
    // TODO: Distinct transitions

    comment("A.4a. Forbid double negation")
    // nodetype[p, NOT] & child_left[p, ch] => ~nodetype[ch, NOT]
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                for (ch in (p + 1)..P)
                    clause(
                        -nodeType[c, k, p, NodeType.NOT.value],
                        -childLeft[c, k, p, ch],
                        -nodeType[c, k, ch, NodeType.NOT.value]
                    )

    return Reduction(
        scenarioTree, C, K, P,
        color, transition, activeTransition, inputEvent, outputEvent, algorithm0, algorithm1,
        nodeType, terminal, parent, childLeft, childRight,
        nodeValue, childValueLeft, childValueRight, firstFired, notFired
    )
}

@Suppress("LocalVariableName")
fun Solver.declareTotalizerExtended(baseReduction: Reduction): IntArray {
    return declareTotalizer(sequence {
        val C = baseReduction.C
        val K = baseReduction.K
        val P = baseReduction.P
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..P)
                    yield(-baseReduction.nodeType[c, k, p, NodeType.NONE.value])
    })
}

fun Solver.declareComparatorExtended(totalizer: IntArray, N: Int, declaredN: Int? = null) {
    declareComparatorLessThanOrEqual(totalizer, N, declaredN)
}

@Suppress("LocalVariableName")
fun Solver.declareCounterExampleExtended(
    baseReduction: Reduction,
    scenarioTree: ScenarioTree,
    counterExampleTree: CounterExampleTree
): CEReduction {
    // Constants
    val C = baseReduction.C
    val K = baseReduction.K
    val P = baseReduction.P
    val V = counterExampleTree.size
    val E = counterExampleTree.inputEvents.size
    val U = counterExampleTree.uniqueInputs.size
    val X = counterExampleTree.uniqueInputs.first().length
    val Z = counterExampleTree.uniqueOutputs.first().length
    // Counterexample variables
    val satisfaction = newArray(V, C + 1)  // denotes the satisfying state (or its absence)
    // Automaton variables
    val transition = baseReduction.transition
    val activeTransition = newArray(C, C, E, U)
    val inputEvent = baseReduction.inputEvent
    val outputEvent = baseReduction.outputEvent
    val algorithm0 = baseReduction.algorithm0
    val algorithm1 = baseReduction.algorithm1
    // Guards variables
    val nodeType = baseReduction.nodeType
    val terminal = baseReduction.terminal
    val childLeft = baseReduction.childLeft
    val childRight = baseReduction.childRight
    val nodeValue = newArray(C, K, P, U)
    val childValueLeft = newArray(C, K, P, U)
    val childValueRight = newArray(C, K, P, U)
    val firstFired = newArray(C, U, K)
    val notFired = newArray(C, U, K)

    comment("CE. Counterexample constraints")
    comment("CE.1. Satisfaction (color-like) constrains")
    comment("CE.1.0. ONE(satisfaction)")
    for (v in 1..V)
        exactlyOne(1..(C + 1), satisfaction, v)

    comment("CE.1.1. Satisfaction of active vertex")
    // satisfaction[tp(v), i] => (satisfaction[v, j] <=> active_transition[i,j,tie(v),tin(v)])
    for (v in counterExampleTree.activeVertices) {
        val p = counterExampleTree.parent(v)
        val e = counterExampleTree.inputEvent(v)
        val u = counterExampleTree.inputNumber(v)
        for (i in 1..C)
            for (j in 1..C)
                implyIff(
                    satisfaction[p, i],
                    satisfaction[v, j],
                    activeTransition[i, j, e, u]
                )
    }

    comment("CE.1.1+. Non-satisfaction of active vertex")
    // satisfaction[tp(v), i] => (satisfaction[v, 0] <=> AND_j( ~active_transition[i,j,tie(v),tin(v)] ))
    for (v in counterExampleTree.activeVertices) {
        val p = counterExampleTree.parent(v)
        val e = counterExampleTree.inputEvent(v)
        val u = counterExampleTree.inputNumber(v)
        for (i in 1..C)
            implyIffAnd(satisfaction[p, i], satisfaction[v, C + 1], sequence {
                for (j in 1..C)
                    yield(-activeTransition[i, j, e, u])
            })
    }

    comment("CE.1.2. Satisfaction of passive vertex")
    // satisfaction[tp(v), c] => (satisfaction[v, c] <=> AND_j( ~active_transition[c,j,tie(v),tin(v)] ))
    for (v in counterExampleTree.passiveVertices) {
        val p = counterExampleTree.parent(v)
        val e = counterExampleTree.inputEvent(v)
        val u = counterExampleTree.inputNumber(v)
        for (c in 1..C)
            implyIffAnd(satisfaction[p, c], satisfaction[v, c], sequence {
                for (j in 1..C)
                    yield(-activeTransition[c, j, e, u])
            })
    }

    comment("CE.1.2+. Non-satisfaction of passive vertex")
    // satisfaction[tp(v), c] => (satisfaction[v, 0] <=> OR_j( active_transition[c,j,tie(v),tin(v)] ))
    for (v in counterExampleTree.passiveVertices) {
        val p = counterExampleTree.parent(v)
        val e = counterExampleTree.inputEvent(v)
        val u = counterExampleTree.inputNumber(v)
        for (c in 1..C)
            implyIffOr(satisfaction[p, c], satisfaction[v, C + 1], sequence {
                for (j in 1..C)
                    yield(activeTransition[c, j, e, u])
            })
    }

    comment("CE.1.3. Propagation of satisfaction for passive vertices")
    // satisfaction[tp(v), c] => satisfaction[v, c] | satisfaction[v, 0]
    for (v in counterExampleTree.passiveVertices) {
        val p = counterExampleTree.parent(v)
        for (c in 1..C)
            clause(-satisfaction[p, c], satisfaction[v, c], satisfaction[v, C + 1])
    }

    comment("CE.1.4. Propagation of non-satisfaction")
    // satisfaction[tp(v), 0] => satisfaction[v, 0]
    for (v in 2..V) {
        val p = counterExampleTree.parent(v)
        imply(satisfaction[p, C + 1], satisfaction[v, C + 1])
    }

    comment("CE.1.5. Forbid loops")
    // satisfaction[v, c] => ~satisfaction[loop(v), c]
    for (v in counterExampleTree.verticesWithLoops) {
        val l = counterExampleTree.loopBack(v)
        println(">>> Forbid loop = $l for v = $v")
        for (c in 1..C)
            imply(satisfaction[v, c], -satisfaction[l, c])
    }

    comment("CE.1.6. Root is satisfied by start state")
    clause(satisfaction[1, 1])


    comment("CE.2. Transition constraints")
    comment("CE.2.1. Active transition definition")
    // active_transition[i,j,e,u] <=> OR_k( transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k] )
    for (i in 1..C)
        for (j in 1..C)
            for (e in 1..E)
                for (u in 1..U)
                    iffOr(activeTransition[i, j, e, u], sequence {
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


    comment("CE.3. Firing constraints")
    comment("CE.3.0. AMO(first_fired)")
    for (c in 1..C)
        for (u in 1..U)
            atMostOne(1..K, firstFired, c, u)


    comment("CE.3.1. not_fired definition")
    // satisfaction[tp(v), c] => not_fired[c,tin(v),K]
    for (v in counterExampleTree.passiveVertices) {
        val p = counterExampleTree.parent(v)
        val u = counterExampleTree.inputNumber(v)
        for (c in 1..C)
            imply(satisfaction[p, c], notFired[c, u, K])
    }

    comment("CE.3.2. not_fired propagation")
    for (c in 1..C)
        for (u in 1..U) {
            // nf[k] => nf[k-1]
            for (k in 2..K)
                imply(notFired[c, u, k], notFired[c, u, k - 1])
            // ~nf[k] => ~nf[k+1]
            for (k in 1..(K - 1))
                imply(-notFired[c, u, k], -notFired[c, u, k + 1])
        }

    comment("CE.3.3. first_fired and not_fired interaction")
    for (c in 1..C)
        for (u in 1..U) {
            // ~(ff & nf)
            for (k in 1..K)
                clause(-firstFired[c, u, k], -notFired[c, u, k])
            // ff[k] => nf[k-1]
            for (k in 2..K)
                imply(firstFired[c, u, k], notFired[c, u, k - 1])
        }

    comment("CE.3.4. Value from not/first fired")
    for (c in 1..C)
        for (u in 1..U)
            for (k in 1..K) {
                // nf => ~root_value
                imply(notFired[c, u, k], -nodeValue[c, k, 1, u])
                // ff => root_value
                imply(firstFired[c, u, k], nodeValue[c, k, 1, u])
                // else => unconstrained
            }


    comment("CE.4. Output event constraints")
    comment("CE.4.1. output_event definition")
    // satisfaction[v, c] => output_event[c, toe(v)]
    for (v in counterExampleTree.activeVertices) {
        val o = counterExampleTree.outputEvent(v)
        for (c in 1..C)
            imply(satisfaction[v, c], outputEvent[c, o])
    }


    comment("CE.5. Algorithm constraints")
    comment("CE.5.2. Algorithms definition")
    for (v in counterExampleTree.activeVertices)
        for (z in 1..Z) {
            val oldValue = counterExampleTree.outputValue(counterExampleTree.parent(v), z)
            val newValue = counterExampleTree.outputValue(v, z)
            for (c in 1..C)
                imply(
                    satisfaction[v, c],
                    when (val values = oldValue to newValue) {
                        false to false -> -algorithm0[c, z]
                        false to true -> algorithm0[c, z]
                        true to false -> -algorithm1[c, z]
                        true to true -> algorithm1[c, z]
                        else -> throw IllegalStateException("Weird combination of values: $values")
                    }
                )
        }


    comment("CE. Guard constraints re-definition for CE unique inputs")

    comment("CE.9.3. None-type nodes have False value and child_values")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (u in 1..U) {
                    imply(
                        nodeType[c, k, p, NodeType.NONE.value],
                        -nodeValue[c, k, p, u]
                    )
                    imply(
                        nodeType[c, k, p, NodeType.NONE.value],
                        -childValueLeft[c, k, p, u]
                    )
                    imply(
                        nodeType[c, k, p, NodeType.NONE.value],
                        -childValueRight[c, k, p, u]
                    )
                }

    comment("CE.10.3. Terminal: child_value_left and child_value_right are False")
    // nodetype[p, TERMINAL] => AND_u( ~child_value_left[p, u] & ~child_value_right[p, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (u in 1..U) {
                    imply(
                        nodeType[c, k, p, NodeType.TERMINAL.value],
                        -childValueLeft[c, k, p, u]
                    )
                    imply(
                        nodeType[c, k, p, NodeType.TERMINAL.value],
                        -childValueRight[c, k, p, u]
                    )
                }

    comment("CE.10.4. Terminals have value from associated input variable")
    // terminal[p, x] => AND_u( value[p, u] <=> u[x] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (x in 1..X)
                    for (u in 1..U)
                        when (val char = counterExampleTree.uniqueInputs[u - 1][x - 1]) {
                            '1' -> imply(terminal[c, k, p, x], nodeValue[c, k, p, u])
                            '0' -> imply(terminal[c, k, p, x], -nodeValue[c, k, p, u])
                            else -> throw IllegalStateException("Character $char is neither '1' nor '0'")
                        }

    comment("CE.11.4a. AND/OR: child_value_left is a value of left child")
    // nodetype[p, AND/OR] & child_left[p, ch] => AND_u( child_value_left[p,u] <=> value[ch, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (ch in (p + 1)..(P - 1))
                    for (u in 1..U)
                        for (nt in sequenceOf(NodeType.AND, NodeType.OR)) {
                            val x1 = nodeType[c, k, p, nt.value]
                            val x2 = childLeft[c, k, p, ch]
                            val x3 = childValueLeft[c, k, p, u]
                            val x4 = nodeValue[c, k, ch, u]
                            clause(-x1, -x2, -x3, x4)
                            clause(-x1, -x2, x3, -x4)
                        }

    comment("CE.11.4b. AND/OR: child_value_right is a value of right child")
    // nodetype[p, AND/OR] & child_right[p, ch] => AND_u( child_value_right[p,u] <=> value[ch, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (ch in (p + 2)..P)
                    for (u in 1..U)
                        for (nt in sequenceOf(NodeType.AND, NodeType.OR)) {
                            val x1 = nodeType[c, k, p, nt.value]
                            val x2 = childRight[c, k, p, ch]
                            val x3 = childValueRight[c, k, p, u]
                            val x4 = nodeValue[c, k, ch, u]
                            clause(-x1, -x2, -x3, x4)
                            clause(-x1, -x2, x3, -x4)
                        }

    comment("CE.11.5a. AND: value is calculated as a conjunction of children")
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

    comment("CE.11.5b. OR: value is calculated as a disjunction of children")
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

    comment("CE.12.4a. NOT: child_value_left is a value of left child")
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

    comment("CE.12.4b. NOT: child_value_right is False")
    // nodetype[p, NOT] => AND_u( ~child_value_right[p, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                for (u in 1..U)
                    imply(
                        nodeType[c, k, p, NodeType.NOT.value],
                        -childValueRight[c, k, p, u]
                    )

    comment("CE.12.5. NOT: value is calculated as a negation of child")
    // nodetype[p, NOT] => AND_u( value[p, u] <=> ~child_value_left[p, u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 1))
                for (u in 1..U) {
                    val x1 = nodeType[c, k, p, NodeType.NOT.value]
                    val x2 = nodeValue[c, k, p, u]
                    val x3 = -childValueLeft[c, k, p, u]
                    clause(-x1, -x2, x3)
                    clause(-x1, x2, -x3)
                }

    comment("CE.A.2. (comb)")
    for (c in 1..C)
        for (k in 1..K)
            for (u in 1..U) {
                // (t!=0 & nf) => ~nodetype[1, NONE]
                clause(
                    transition[c, k, C + 1],
                    -notFired[c, u, k],
                    -nodeType[c, k, 1, NodeType.NONE.value]
                )
                // ff => ~nodetype[1, NONE]
                imply(
                    firstFired[c, u, k],
                    -nodeType[c, k, 1, NodeType.NONE.value]
                )
            }


    comment("CE.+. Equal variables for same inputs in both trees")
    for (input in counterExampleTree.uniqueInputs.intersect(scenarioTree.uniqueInputs)) {
        val uCE = counterExampleTree.uniqueInputs.indexOf(input) + 1
        val uST = scenarioTree.uniqueInputs.indexOf(input) + 1

        for (c in 1..C) {
            for (j in 1..C) {
                for (e in 1..E)
                    iff(
                        activeTransition[c, j, e, uCE],
                        baseReduction.activeTransition[c, j, e, uST]
                    )
            }
            for (k in 1..K) {
                for (p in 1..P) {
                    iff(
                        nodeValue[c, k, p, uCE],
                        baseReduction.nodeValue[c, k, p, uST]
                    )
                    iff(
                        childValueLeft[c, k, p, uCE],
                        baseReduction.childValueLeft[c, k, p, uST]
                    )
                    iff(
                        childValueRight[c, k, p, uCE],
                        baseReduction.childValueRight[c, k, p, uST]
                    )
                }
                iff(
                    firstFired[c, uCE, k],
                    baseReduction.firstFired[c, uST, k]
                )
                iff(
                    notFired[c, uCE, k],
                    baseReduction.notFired[c, uST, k]
                )
            }
        }
    }

    comment("CE. Ad-hoc: reverse-implication (even iff)")
    // ff[k] <=> root_value[k] & AND_{k'<k}( ~ff[k'] )
    for (c in 1..C)
        for (input in counterExampleTree.uniqueInputs - scenarioTree.uniqueInputs) {
            val u = counterExampleTree.uniqueInputs.indexOf(input) + 1
            for (k in 1..K)
                iffAnd(firstFired[c, u, k], sequence {
                    yield(nodeValue[c, k, 1, u])
                    for (k_ in 1 until k)
                        yield(-firstFired[c, u, k_])
                })
        }

    return CEReduction(
        counterExampleTree, C, K, P,
        satisfaction, activeTransition,
        nodeValue, childValueLeft, childValueRight, firstFired, notFired
    )
}
