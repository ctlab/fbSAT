// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++, C#, and Java: http://www.viva64.com

package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.automaton.NodeType
import ru.ifmo.fbsat.scenario.CounterExampleTree
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.solver.declareAtMostOne
import ru.ifmo.fbsat.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.solver.declareExactlyOne
import ru.ifmo.fbsat.solver.declareIff
import ru.ifmo.fbsat.solver.declareIffAnd
import ru.ifmo.fbsat.solver.declareIffOr
import ru.ifmo.fbsat.solver.declareImply
import ru.ifmo.fbsat.solver.declareImplyIff
import ru.ifmo.fbsat.solver.declareImplyIffAnd
import ru.ifmo.fbsat.solver.declareImplyIffOr
import ru.ifmo.fbsat.solver.declareTotalizer
import ru.ifmo.fbsat.utils.IntMultiArray

/**
 * EXTENDED method
 */
internal class Reduction(
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
) {
    companion object {
        @Suppress("LocalVariableName")
        fun declareBaseReduction(solver: Solver, scenarioTree: ScenarioTree, C: Int, K: Int, P: Int): Reduction {
            // Constants
            val V = scenarioTree.size
            val E = scenarioTree.inputEvents.size
            val O = scenarioTree.outputEvents.size
            val U = scenarioTree.uniqueInputs.size
            val X = scenarioTree.uniqueInputs.first().length
            val Z = scenarioTree.uniqueOutputs.first().length
            // Automaton variables
            val color = solver.newArray(V, C)
            val transition = solver.newArray(C, K, C + 1)
            val activeTransition = solver.newArray(C, C, E, U)
            val inputEvent = solver.newArray(C, K, E + 1)
            val outputEvent = solver.newArray(C, O)
            val algorithm0 = solver.newArray(C, Z)
            val algorithm1 = solver.newArray(C, Z)
            // Guards variables
            val nodeType = solver.newArray(C, K, P, 5)
            val terminal = solver.newArray(C, K, P, X + 1)
            val parent = solver.newArray(C, K, P, P + 1)
            val childLeft = solver.newArray(C, K, P, P + 1)
            val childRight = solver.newArray(C, K, P, P + 1)
            val nodeValue = solver.newArray(C, K, P, U)
            val childValueLeft = solver.newArray(C, K, P, U)
            val childValueRight = solver.newArray(C, K, P, U)
            val firstFired = solver.newArray(C, U, K)
            val notFired = solver.newArray(C, U, K)
            // BFS variables
            val bfs_transition = solver.newArray(C, C)
            val bfs_parent = solver.newArray(C, C)


            solver.addComment("1. Color constraints")
            solver.addComment("1.0. ONE(color)")
            for (v in 1..V)
                solver.declareExactlyOne(1..C, color, v)

            solver.addComment("1.1. Color propagation for passive vertices")
            // color[tp(v), c] => color[v, c]
            for (v in scenarioTree.passiveVertices) {
                val p = scenarioTree.parent(v)
                for (c in 1..C)
                    solver.declareImply(color[p, c], color[v, c])
            }

            solver.addComment("1.2. Root corresponds to start state")
            solver.addClause(color[1, 1])


            solver.addComment("2. Transition constraints")
            solver.addComment("2.0a. ONE(transition)")
            for (c in 1..C)
                for (k in 1..K)
                    solver.declareExactlyOne(1..(C + 1), transition, c, k)

            solver.addComment("2.0b. ONE(input_event)")
            for (c in 1..C)
                for (k in 1..K)
                    solver.declareExactlyOne(1..(E + 1), inputEvent, c, k)

            solver.addComment("2.1. Active transition existence")
            // color[tp(v), i] & color[v, j] => active_transition[i,j,tie(v),tin(v)]
            for (v in scenarioTree.activeVertices) {
                val p = scenarioTree.parent(v)
                val e = scenarioTree.inputEvent(v)
                val u = scenarioTree.inputNumber(v)
                for (i in 1..C)
                    for (j in 1..C)
                        solver.addClause(
                            -color[p, i],
                            -color[v, j],
                            activeTransition[i, j, e, u]
                        )
            }

            solver.addComment("2.2. Active transition definition")
            // active_transition[i,j,e,u] <=> OR_k( transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k] )
            for (i in 1..C)
                for (j in 1..C)
                    for (e in 1..E)
                        for (u in 1..U)
                            solver.declareIffOr(activeTransition[i, j, e, u], sequence {
                                for (k in 1..K) {
                                    // aux <=> transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k]
                                    val aux = solver.newVariable()
                                    solver.declareIffAnd(
                                        aux,
                                        transition[i, k, j],
                                        inputEvent[i, k, e],
                                        firstFired[i, u, k]
                                    )
                                    yield(aux)
                                }
                            })

            solver.addComment("2.3. Null-transitions are last")
            // transition[k, 0] => transition[k+1, 0]
            for (c in 1..C)
                for (k in 1..(K - 1))
                    solver.declareImply(transition[c, k, C + 1], transition[c, k + 1, C + 1])

            solver.addComment("2.4. Only null-transitions have no input event")
            // transition[k, 0] <=> input_event[k, 0]
            for (c in 1..C)
                for (k in 1..K)
                    solver.declareIff(transition[c, k, C + 1], inputEvent[c, k, E + 1])


            solver.addComment("3. Firing constraints")
            solver.addComment("3.0. AMO(first_fired)")
            for (c in 1..C)
                for (u in 1..U)
                    solver.declareAtMostOne(1..K, firstFired, c, u)

            solver.addComment("3.1. not_fired definition")
            // color[tp(v), c] => not_fired[c, tin(v), K]
            for (v in scenarioTree.passiveVertices) {
                val p = scenarioTree.parent(v)
                val u = scenarioTree.inputNumber(v)
                for (c in 1..C)
                    solver.declareImply(color[p, c], notFired[c, u, K])
            }

            solver.addComment("3.2. not_fired propagation")
            for (c in 1..C)
                for (u in 1..U) {
                    // nf[k] => nf[k-1]
                    for (k in 2..K)
                        solver.declareImply(notFired[c, u, k], notFired[c, u, k - 1])
                    // ~nf[k] => ~nf[k+1]
                    for (k in 1..(K - 1))
                        solver.declareImply(-notFired[c, u, k], -notFired[c, u, k + 1])
                }

            solver.addComment("3.3. first_fired and not_fired interaction")
            for (c in 1..C)
                for (u in 1..U) {
                    // ~(ff & nf)
                    for (k in 1..K)
                        solver.addClause(-firstFired[c, u, k], -notFired[c, u, k])
                    // ff[k] => nf[k-1]
                    for (k in 2..K)
                        solver.declareImply(firstFired[c, u, k], notFired[c, u, k - 1])
                }

            solver.addComment("3.4. Value from not/first fired")
            for (c in 1..C)
                for (u in 1..U)
                    for (k in 1..K) {
                        // nf => ~root_value
                        solver.declareImply(notFired[c, u, k], -nodeValue[c, k, 1, u])
                        // ff => root_value
                        solver.declareImply(firstFired[c, u, k], nodeValue[c, k, 1, u])
                        // else => unconstrained
                    }


            solver.addComment("4. Output event constraints")
            solver.addComment("4.0. ONE(output_event)")
            for (c in 1..C)
                solver.declareExactlyOne(1..O, outputEvent, c)

            solver.addComment("4.1. output_event definition")
            // color[v, c] => output_event[c, toe(v)]
            for (v in scenarioTree.activeVertices) {
                val o = scenarioTree.outputEvent(v)
                for (c in 1..C)
                    solver.declareImply(color[v, c], outputEvent[c, o])
            }

            solver.addComment("4.2. Start state does INITO (root`s output event)")
            solver.addClause(outputEvent[1, scenarioTree.outputEvent(1)])


            solver.addComment("5. Algorithm constraints")
            solver.addComment("5.1. Start state does nothing")
            for (z in 1..Z) {
                solver.addClause(-algorithm0[1, z])
                solver.addClause(algorithm1[1, z])
            }

            solver.addComment("5.2. Algorithms definition")
            for (v in scenarioTree.activeVertices)
                for (z in 1..Z) {
                    val oldValue = scenarioTree.outputValue(scenarioTree.parent(v), z)
                    val newValue = scenarioTree.outputValue(v, z)
                    for (c in 1..C)
                        solver.declareImply(
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


            solver.addComment("6. BFS constraints")
            solver.addComment("6.1. F_t")
            // t[i, j] <=> OR_k( transition[i,k,j] )
            for (i in 1..C)
                for (j in 1..C)
                    solver.declareIffOr(bfs_transition[i, j], sequence {
                        for (k in 1..K)
                            yield(transition[i, k, j])
                    })

            solver.addComment("6.2. F_p")
            // p[j, i] <=> t[i, j] & AND_{k<i}( ~t[k, j] )
            for (i in 1..C) {
                // to avoid ambiguous unused variable:
                for (j in 1..i)
                    solver.addClause(-bfs_parent[j, i])

                for (j in (i + 1)..C)
                    solver.declareIffAnd(bfs_parent[j, i],
                        sequence {
                            yield(bfs_transition[i, j])
                            for (k in 1..(i - 1))
                                yield(-bfs_transition[k, j])
                        })
            }

            solver.addComment("6.3. F_ALO(p)")
            for (j in 2..C)
                solver.addClause(sequence {
                    for (i in 1..(j - 1))
                        yield(bfs_parent[j, i])
                })

            solver.addComment("6.4. F_BFS(p)")
            // p[j, i] => ~p[j+1, k]
            for (k in 1..C)
                for (i in (k + 1)..C)
                    for (j in (i + 1)..(C - 1))
                        solver.declareImply(bfs_parent[j, i], -bfs_parent[j + 1, k])


            solver.addComment("7. Nodetype constraints")
            solver.addComment("7.0. ONE(nodetype)")
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        solver.declareExactlyOne(1..5, nodeType, c, k, p)


            solver.addComment("8. Parent and children constraints")
            solver.addComment("8.0a. ONE(parent)")
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        solver.declareExactlyOne(1..(P + 1), parent, c, k, p)

            solver.addComment("8.0b. ONE(child_left)")
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        solver.declareExactlyOne(1..(P + 1), childLeft, c, k, p)

            solver.addComment("8.0c. ONE(child_right)")
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        solver.declareExactlyOne(1..(P + 1), childRight, c, k, p)

            solver.addComment("8.1. parent<->child relation")
            // parent[ch, p] => (child_left[p, ch] | child_right[p, ch])
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 1))
                        for (ch in (p + 1)..P)
                            solver.addClause(
                                -parent[c, k, ch, p],
                                childLeft[c, k, p, ch],
                                childRight[c, k, p, ch]
                            )

            solver.addComment("8.2. Typed nodes (except root) have parent with lesser number")
            // ~nodetype[p, NONE]  =>  OR_par( parent[p, par] )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 2..P)
                        solver.addClause(sequence {
                            yield(nodeType[c, k, p, NodeType.NONE.value])
                            for (par in 1..(p - 1))
                                yield(parent[c, k, p, par])
                        })

            solver.addComment("8.3. Root has no parent")
            for (c in 1..C)
                for (k in 1..K)
                    solver.addClause(parent[c, k, 1, P + 1])


            solver.addComment("9. None-type nodes constraints")
            solver.addComment("9.1. None-type nodes have largest numbers")
            // nodetype[p, NONE] => nodetype[p+1, NONE]
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 1))
                        solver.declareImply(
                            nodeType[c, k, p, NodeType.NONE.value],
                            nodeType[c, k, p + 1, NodeType.NONE.value]
                        )

            solver.addComment("9.2. None-type nodes have no parent and no children")
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P) {
                        solver.declareImply(
                            nodeType[c, k, p, NodeType.NONE.value],
                            parent[c, k, p, P + 1]
                        )
                        solver.declareImply(
                            nodeType[c, k, p, NodeType.NONE.value],
                            childLeft[c, k, p, P + 1]
                        )
                        solver.declareImply(
                            nodeType[c, k, p, NodeType.NONE.value],
                            childRight[c, k, p, P + 1]
                        )
                    }

            solver.addComment("9.3. None-type nodes have False value and child_values")
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        for (u in 1..U) {
                            solver.declareImply(
                                nodeType[c, k, p, NodeType.NONE.value],
                                -nodeValue[c, k, p, u]
                            )
                            solver.declareImply(
                                nodeType[c, k, p, NodeType.NONE.value],
                                -childValueLeft[c, k, p, u]
                            )
                            solver.declareImply(
                                nodeType[c, k, p, NodeType.NONE.value],
                                -childValueRight[c, k, p, u]
                            )
                        }


            solver.addComment("10. Terminals constraints")
            solver.addComment("10.0. ONE(terminal)")
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        solver.declareExactlyOne(1..(X + 1), terminal, c, k, p)

            solver.addComment("10.1. Only terminals have associated terminal variables")
            // nodetype[p, TERMINAL] <=> -terminal[p, 0]
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        solver.declareIff(
                            nodeType[c, k, p, NodeType.TERMINAL.value],
                            -terminal[c, k, p, X + 1]
                        )

            solver.addComment("10.2. Terminals have no children")
            // nodetype[p, TERMINAL] => child_left[p, 0] & child_right[p, 0]
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P) {
                        solver.declareImply(
                            nodeType[c, k, p, NodeType.TERMINAL.value],
                            childLeft[c, k, p, P + 1]
                        )
                        solver.declareImply(
                            nodeType[c, k, p, NodeType.TERMINAL.value],
                            childRight[c, k, p, P + 1]
                        )
                    }

            solver.addComment("10.3. Terminal: child_value_left and child_value_right are False")
            // nodetype[p, TERMINAL] => AND_u( ~child_value_left[p, u] & ~child_value_right[p, u] )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        for (u in 1..U) {
                            solver.declareImply(
                                nodeType[c, k, p, NodeType.TERMINAL.value],
                                -childValueLeft[c, k, p, u]
                            )
                            solver.declareImply(
                                nodeType[c, k, p, NodeType.TERMINAL.value],
                                -childValueRight[c, k, p, u]
                            )
                        }

            solver.addComment("10.4. Terminals have value from associated input variable")
            // terminal[p, x] => AND_u( value[p, u] <=> u[x] )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        for (x in 1..X)
                            for (u in 1..U)
                                when (val char = scenarioTree.uniqueInputs[u - 1][x - 1]) {
                                    '1' -> solver.declareImply(terminal[c, k, p, x], nodeValue[c, k, p, u])
                                    '0' -> solver.declareImply(terminal[c, k, p, x], -nodeValue[c, k, p, u])
                                    else -> throw IllegalStateException("Character $char is neither '1' nor '0'")
                                }


            solver.addComment("11. AND/OR nodes constraints")
            solver.addComment("11.0. AND/OR nodes cannot have numbers P-1 or P")
            for (c in 1..C)
                for (k in 1..K) {
                    if (P >= 1) {
                        solver.addClause(-nodeType[c, k, P, NodeType.AND.value])
                        solver.addClause(-nodeType[c, k, P, NodeType.OR.value])
                    }
                    if (P >= 2) {
                        solver.addClause(-nodeType[c, k, P - 1, NodeType.AND.value])
                        solver.addClause(-nodeType[c, k, P - 1, NodeType.OR.value])
                    }
                }

            solver.addComment("11.1. AND/OR: left child has greater number")
            // nodetype[p, AND/OR] => OR_{ch in (p+1)..(P-1)}( child_left[p, ch] )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 2))
                        for (nt in sequenceOf(NodeType.AND, NodeType.OR))
                            solver.addClause(sequence {
                                yield(-nodeType[c, k, p, nt.value])
                                for (ch in (p + 1)..(P - 1))
                                    yield(childLeft[c, k, p, ch])
                            })

            solver.addComment("11.2. AND/OR: right child is adjacent (+1) to left")
            // nodetype[p, AND/OR] & child_left[p, ch] => child_right[p, ch+1]
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 2))
                        for (ch in (p + 1)..(P - 1))
                            for (nt in sequenceOf(NodeType.AND, NodeType.OR))
                                solver.addClause(
                                    -nodeType[c, k, p, nt.value],
                                    -childLeft[c, k, p, ch],
                                    childRight[c, k, p, ch + 1]
                                )

            solver.addComment("11.3. AND/OR: children`s parents")
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
                                solver.addClause(-x1, -x2, x3)
                                solver.addClause(-x1, -x2, x4)
                            }

            solver.addComment("11.4a. AND/OR: child_value_left is a value of left child")
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
                                    solver.addClause(-x1, -x2, -x3, x4)
                                    solver.addClause(-x1, -x2, x3, -x4)
                                }

            solver.addComment("11.4b. AND/OR: child_value_right is a value of right child")
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
                                    solver.addClause(-x1, -x2, -x3, x4)
                                    solver.addClause(-x1, -x2, x3, -x4)
                                }

            solver.addComment("11.5a. AND: value is calculated as a conjunction of children")
            // nodetype[p, AND] => AND_u(value[p, u] <=> (child_value_left[p, u] & child_value_right[p, u]))
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 2))
                        for (u in 1..U) {
                            val x1 = nodeType[c, k, p, NodeType.AND.value]
                            val x2 = nodeValue[c, k, p, u]
                            val x3 = childValueLeft[c, k, p, u]
                            val x4 = childValueRight[c, k, p, u]
                            solver.addClause(-x1, x2, -x3, -x4)
                            solver.addClause(-x1, -x2, x3)
                            solver.addClause(-x1, -x2, x4)
                        }

            solver.addComment("11.5b. OR: value is calculated as a disjunction of children")
            // nodetype[p, OR] => AND_u( value[p, u] <=> (child_value_left[p, u] | child_value_right[p, u]) )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 2))
                        for (u in 1..U) {
                            val x1 = nodeType[c, k, p, NodeType.OR.value]
                            val x2 = nodeValue[c, k, p, u]
                            val x3 = childValueLeft[c, k, p, u]
                            val x4 = childValueRight[c, k, p, u]
                            solver.addClause(-x1, -x2, x3, x4)
                            solver.addClause(-x1, x2, -x3)
                            solver.addClause(-x1, x2, -x4)
                        }


            solver.addComment("12. NOT nodes constraints")
            solver.addComment("12.0. NOT nodes cannot have number P")
            for (c in 1..C)
                for (k in 1..K)
                    solver.addClause(-nodeType[c, k, P, NodeType.NOT.value])

            solver.addComment("12.1. NOT: left child has greater number")
            // nodetype[p, NOT] => OR_{ch in (p+1)..P}( child_left[p, ch] )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 1))
                        solver.addClause(sequence {
                            yield(-nodeType[c, k, p, NodeType.NOT.value])
                            for (ch in (p + 1)..P)
                                yield(childLeft[c, k, p, ch])
                        })

            solver.addComment("12.2. NOT: child`s parents")
            // nodetype[p, NOT] & child_left[p, ch] => parent[ch, p]
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 1))
                        for (ch in (p + 1)..P)
                            solver.addClause(
                                -nodeType[c, k, p, NodeType.NOT.value],
                                -childLeft[c, k, p, ch],
                                parent[c, k, ch, p]
                            )

            solver.addComment("12.3. NOT: no right child")
            // nodetype[p, NOT] => child_right[p, 0]
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 1))
                        solver.declareImply(
                            nodeType[c, k, p, NodeType.NOT.value],
                            childRight[c, k, p, P + 1]
                        )

            solver.addComment("12.4a. NOT: child_value_left is a value of left child")
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
                                solver.addClause(-x1, -x2, -x3, x4)
                                solver.addClause(-x1, -x2, x3, -x4)
                            }

            solver.addComment("12.4b. NOT: child_value_right is False")
            // nodetype[p, NOT] => AND_u( ~child_value_right[p, u] )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 1))
                        for (u in 1..U)
                            solver.declareImply(
                                nodeType[c, k, p, NodeType.NOT.value],
                                -childValueRight[c, k, p, u]
                            )

            solver.addComment("12.5. NOT: value is calculated as a negation of child")
            // nodetype[p, NOT] => AND_u( value[p, u] <=> ~child_value_left[p, u] )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 1))
                        for (u in 1..U) {
                            val x1 = nodeType[c, k, p, NodeType.NOT.value]
                            val x2 = nodeValue[c, k, p, u]
                            val x3 = -childValueLeft[c, k, p, u]
                            solver.addClause(-x1, -x2, x3)
                            solver.addClause(-x1, x2, -x3)
                        }


            solver.addComment("A. AD-HOCs")
            solver.addComment("A.1. (comb)")
            // t=0 <=> nodetype[1, NONE]
            for (c in 1..C)
                for (k in 1..K)
                    solver.declareIff(transition[c, k, C + 1], nodeType[c, k, 1, NodeType.NONE.value])

            solver.addComment("A.2. (comb)")
            for (c in 1..C)
                for (k in 1..K)
                    for (u in 1..U) {
                        // (t!=0 & nf) => ~nodetype[1, NONE]
                        solver.addClause(
                            transition[c, k, C + 1],
                            -notFired[c, u, k],
                            -nodeType[c, k, 1, NodeType.NONE.value]
                        )
                        // ff => ~nodetype[1, NONE]
                        solver.declareImply(
                            firstFired[c, u, k],
                            -nodeType[c, k, 1, NodeType.NONE.value]
                        )
                    }

            solver.addComment("A.3. Distinct transitions")
            // TODO: Distinct transitions

            solver.addComment("A.4a. Forbid double negation")
            // nodetype[p, NOT] & child_left[p, ch] => ~nodetype[ch, NOT]
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 1))
                        for (ch in (p + 1)..P)
                            solver.addClause(
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
        fun declareTotalizer(solver: Solver, baseReduction: Reduction): IntArray {
            return solver.declareTotalizer(sequence {
                val C = baseReduction.C
                val K = baseReduction.K
                val P = baseReduction.P
                for (c in 1..C)
                    for (k in 1..K)
                        for (p in 1..P)
                            yield(-baseReduction.nodeType[c, k, p, NodeType.NONE.value])
            })
        }

        fun declareComparator(solver: Solver, totalizer: IntArray, N: Int, declaredN: Int? = null) {
            solver.declareComparatorLessThanOrEqual(totalizer, N, declaredN)
        }

        @Suppress("LocalVariableName")
        fun declareCE(
            solver: Solver,
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
            // Automaton variables
            val transition = baseReduction.transition
            val activeTransition = solver.newArray(C, C, E, U)
            val inputEvent = baseReduction.inputEvent
            val outputEvent = baseReduction.outputEvent
            val algorithm0 = baseReduction.algorithm0
            val algorithm1 = baseReduction.algorithm1
            // Guards variables
            val nodeType = baseReduction.nodeType
            val terminal = baseReduction.terminal
            val childLeft = baseReduction.childLeft
            val childRight = baseReduction.childRight
            val nodeValue = solver.newArray(C, K, P, U)
            val childValueLeft = solver.newArray(C, K, P, U)
            val childValueRight = solver.newArray(C, K, P, U)
            val firstFired = solver.newArray(C, U, K)
            val notFired = solver.newArray(C, U, K)
            // Counterexample variables
            val satisfaction = solver.newArray(V, C + 1)  // denotes the satisfying state (or its absence)


            solver.addComment("CE. Counterexample constraints")
            solver.addComment("CE.1. Satisfaction (color-like) constrains")
            solver.addComment("CE.1.0. ONE(satisfaction)")
            for (v in 1..V)
                solver.declareExactlyOne(1..(C + 1), satisfaction, v)

            solver.addComment("CE.1.1. Satisfaction of active vertex")
            // satisfaction[tp(v), i] => (satisfaction[v, j] <=> active_transition[i,j,tie(v),tin(v)])
            for (v in counterExampleTree.activeVertices) {
                val p = counterExampleTree.parent(v)
                val e = counterExampleTree.inputEvent(v)
                val u = counterExampleTree.inputNumber(v)
                for (i in 1..C)
                    for (j in 1..C)
                        solver.declareImplyIff(
                            satisfaction[p, i],
                            satisfaction[v, j],
                            activeTransition[i, j, e, u]
                        )
            }

            solver.addComment("CE.1.1+. Non-satisfaction of active vertex")
            // satisfaction[tp(v), i] => (satisfaction[v, 0] <=> AND_j( ~active_transition[i,j,tie(v),tin(v)] ))
            for (v in counterExampleTree.activeVertices) {
                val p = counterExampleTree.parent(v)
                val e = counterExampleTree.inputEvent(v)
                val u = counterExampleTree.inputNumber(v)
                for (i in 1..C)
                    solver.declareImplyIffAnd(satisfaction[p, i], satisfaction[v, C + 1], sequence {
                        for (j in 1..C)
                            yield(-activeTransition[i, j, e, u])
                    })
            }

            solver.addComment("CE.1.2. Satisfaction of passive vertex")
            // satisfaction[tp(v), c] => (satisfaction[v, c] <=> AND_j( ~active_transition[c,j,tie(v),tin(v)] ))
            for (v in counterExampleTree.passiveVertices) {
                val p = counterExampleTree.parent(v)
                val e = counterExampleTree.inputEvent(v)
                val u = counterExampleTree.inputNumber(v)
                for (c in 1..C)
                    solver.declareImplyIffAnd(satisfaction[p, c], satisfaction[v, c], sequence {
                        for (j in 1..C)
                            yield(-activeTransition[c, j, e, u])
                    })
            }

            solver.addComment("CE.1.2+. Non-satisfaction of passive vertex")
            // satisfaction[tp(v), c] => (satisfaction[v, 0] <=> OR_j( active_transition[c,j,tie(v),tin(v)] ))
            for (v in counterExampleTree.passiveVertices) {
                val p = counterExampleTree.parent(v)
                val e = counterExampleTree.inputEvent(v)
                val u = counterExampleTree.inputNumber(v)
                for (c in 1..C)
                    solver.declareImplyIffOr(satisfaction[p, c], satisfaction[v, C + 1], sequence {
                        for (j in 1..C)
                            yield(activeTransition[c, j, e, u])
                    })
            }

            solver.addComment("CE.1.3. Propagation of satisfaction for passive vertices")
            // satisfaction[tp(v), c] => satisfaction[v, c] | satisfaction[v, 0]
            for (v in counterExampleTree.passiveVertices) {
                val p = counterExampleTree.parent(v)
                for (c in 1..C)
                    solver.addClause(-satisfaction[p, c], satisfaction[v, c], satisfaction[v, C + 1])
            }

            solver.addComment("CE.1.4. Propagation of non-satisfaction")
            // satisfaction[tp(v), 0] => satisfaction[v, 0]
            for (v in 2..V) {
                val p = counterExampleTree.parent(v)
                solver.declareImply(satisfaction[p, C + 1], satisfaction[v, C + 1])
            }

            solver.addComment("CE.1.5. Forbid loops")
            // satisfaction[v, c] => ~satisfaction[loop(v), c]
            for (v in counterExampleTree.verticesWithLoops) {
                val l = counterExampleTree.loopBack(v)
                println(">>> Forbid loop = $l for v = $v")
                for (c in 1..C)
                    solver.declareImply(satisfaction[v, c], -satisfaction[l, c])
            }

            solver.addComment("CE.1.6. Root is satisfied by start state")
            solver.addClause(satisfaction[1, 1])


            solver.addComment("CE.2. Transition constraints")
            solver.addComment("CE.2.1. Active transition definition")
            // active_transition[i,j,e,u] <=> OR_k( transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k] )
            for (i in 1..C)
                for (j in 1..C)
                    for (e in 1..E)
                        for (u in 1..U)
                            solver.declareIffOr(activeTransition[i, j, e, u], sequence {
                                for (k in 1..K) {
                                    // aux <=> transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k]
                                    val aux = solver.newVariable()
                                    solver.declareIffAnd(
                                        aux,
                                        transition[i, k, j],
                                        inputEvent[i, k, e],
                                        firstFired[i, u, k]
                                    )
                                    yield(aux)
                                }
                            })


            solver.addComment("CE.3. Firing constraints")
            solver.addComment("CE.3.0. AMO(first_fired)")
            for (c in 1..C)
                for (u in 1..U)
                    solver.declareAtMostOne(1..K, firstFired, c, u)


            solver.addComment("CE.3.1. not_fired definition")
            // satisfaction[tp(v), c] => not_fired[c,tin(v),K]
            for (v in counterExampleTree.passiveVertices) {
                val p = counterExampleTree.parent(v)
                val u = counterExampleTree.inputNumber(v)
                for (c in 1..C)
                    solver.declareImply(satisfaction[p, c], notFired[c, u, K])
            }

            solver.addComment("CE.3.2. not_fired propagation")
            for (c in 1..C)
                for (u in 1..U) {
                    // nf[k] => nf[k-1]
                    for (k in 2..K)
                        solver.declareImply(notFired[c, u, k], notFired[c, u, k - 1])
                    // ~nf[k] => ~nf[k+1]
                    for (k in 1..(K - 1))
                        solver.declareImply(-notFired[c, u, k], -notFired[c, u, k + 1])
                }

            solver.addComment("CE.3.3. first_fired and not_fired interaction")
            for (c in 1..C)
                for (u in 1..U) {
                    // ~(ff & nf)
                    for (k in 1..K)
                        solver.addClause(-firstFired[c, u, k], -notFired[c, u, k])
                    // ff[k] => nf[k-1]
                    for (k in 2..K)
                        solver.declareImply(firstFired[c, u, k], notFired[c, u, k - 1])
                }

            solver.addComment("CE.3.4. Value from not/first fired")
            for (c in 1..C)
                for (u in 1..U)
                    for (k in 1..K) {
                        // nf => ~root_value
                        solver.declareImply(notFired[c, u, k], -nodeValue[c, k, 1, u])
                        // ff => root_value
                        solver.declareImply(firstFired[c, u, k], nodeValue[c, k, 1, u])
                        // else => unconstrained
                    }


            solver.addComment("CE.4. Output event constraints")
            solver.addComment("CE.4.1. output_event definition")
            // satisfaction[v, c] => output_event[c, toe(v)]
            for (v in counterExampleTree.activeVertices) {
                val o = counterExampleTree.outputEvent(v)
                for (c in 1..C)
                    solver.declareImply(satisfaction[v, c], outputEvent[c, o])
            }


            solver.addComment("CE.5. Algorithm constraints")
            solver.addComment("CE.5.2. Algorithms definition")
            for (v in counterExampleTree.activeVertices)
                for (z in 1..Z) {
                    val oldValue = counterExampleTree.outputValue(counterExampleTree.parent(v), z)
                    val newValue = counterExampleTree.outputValue(v, z)
                    for (c in 1..C)
                        solver.declareImply(
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


            solver.addComment("CE. Guard constraints re-definition for CE unique inputs")

            solver.addComment("CE.9.3. None-type nodes have False value and child_values")
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        for (u in 1..U) {
                            solver.declareImply(
                                nodeType[c, k, p, NodeType.NONE.value],
                                -nodeValue[c, k, p, u]
                            )
                            solver.declareImply(
                                nodeType[c, k, p, NodeType.NONE.value],
                                -childValueLeft[c, k, p, u]
                            )
                            solver.declareImply(
                                nodeType[c, k, p, NodeType.NONE.value],
                                -childValueRight[c, k, p, u]
                            )
                        }

            solver.addComment("CE.10.3. Terminal: child_value_left and child_value_right are False")
            // nodetype[p, TERMINAL] => AND_u( ~child_value_left[p, u] & ~child_value_right[p, u] )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        for (u in 1..U) {
                            solver.declareImply(
                                nodeType[c, k, p, NodeType.TERMINAL.value],
                                -childValueLeft[c, k, p, u]
                            )
                            solver.declareImply(
                                nodeType[c, k, p, NodeType.TERMINAL.value],
                                -childValueRight[c, k, p, u]
                            )
                        }

            solver.addComment("CE.10.4. Terminals have value from associated input variable")
            // terminal[p, x] => AND_u( value[p, u] <=> u[x] )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        for (x in 1..X)
                            for (u in 1..U)
                                when (val char = counterExampleTree.uniqueInputs[u - 1][x - 1]) {
                                    '1' -> solver.declareImply(terminal[c, k, p, x], nodeValue[c, k, p, u])
                                    '0' -> solver.declareImply(terminal[c, k, p, x], -nodeValue[c, k, p, u])
                                    else -> throw IllegalStateException("Character $char is neither '1' nor '0'")
                                }

            solver.addComment("CE.11.4a. AND/OR: child_value_left is a value of left child")
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
                                    solver.addClause(-x1, -x2, -x3, x4)
                                    solver.addClause(-x1, -x2, x3, -x4)
                                }

            solver.addComment("CE.11.4b. AND/OR: child_value_right is a value of right child")
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
                                    solver.addClause(-x1, -x2, -x3, x4)
                                    solver.addClause(-x1, -x2, x3, -x4)
                                }

            solver.addComment("CE.11.5a. AND: value is calculated as a conjunction of children")
            // nodetype[p, AND] => AND_u(value[p, u] <=> (child_value_left[p, u] & child_value_right[p, u]))
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 2))
                        for (u in 1..U) {
                            val x1 = nodeType[c, k, p, NodeType.AND.value]
                            val x2 = nodeValue[c, k, p, u]
                            val x3 = childValueLeft[c, k, p, u]
                            val x4 = childValueRight[c, k, p, u]
                            solver.addClause(-x1, x2, -x3, -x4)
                            solver.addClause(-x1, -x2, x3)
                            solver.addClause(-x1, -x2, x4)
                        }

            solver.addComment("CE.11.5b. OR: value is calculated as a disjunction of children")
            // nodetype[p, OR] => AND_u( value[p, u] <=> (child_value_left[p, u] | child_value_right[p, u]) )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 2))
                        for (u in 1..U) {
                            val x1 = nodeType[c, k, p, NodeType.OR.value]
                            val x2 = nodeValue[c, k, p, u]
                            val x3 = childValueLeft[c, k, p, u]
                            val x4 = childValueRight[c, k, p, u]
                            solver.addClause(-x1, -x2, x3, x4)
                            solver.addClause(-x1, x2, -x3)
                            solver.addClause(-x1, x2, -x4)
                        }

            solver.addComment("CE.12.4a. NOT: child_value_left is a value of left child")
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
                                solver.addClause(-x1, -x2, -x3, x4)
                                solver.addClause(-x1, -x2, x3, -x4)
                            }

            solver.addComment("CE.12.4b. NOT: child_value_right is False")
            // nodetype[p, NOT] => AND_u( ~child_value_right[p, u] )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 1))
                        for (u in 1..U)
                            solver.declareImply(
                                nodeType[c, k, p, NodeType.NOT.value],
                                -childValueRight[c, k, p, u]
                            )

            solver.addComment("CE.12.5. NOT: value is calculated as a negation of child")
            // nodetype[p, NOT] => AND_u( value[p, u] <=> ~child_value_left[p, u] )
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..(P - 1))
                        for (u in 1..U) {
                            val x1 = nodeType[c, k, p, NodeType.NOT.value]
                            val x2 = nodeValue[c, k, p, u]
                            val x3 = -childValueLeft[c, k, p, u]
                            solver.addClause(-x1, -x2, x3)
                            solver.addClause(-x1, x2, -x3)
                        }

            solver.addComment("CE.A.2. (comb)")
            for (c in 1..C)
                for (k in 1..K)
                    for (u in 1..U) {
                        // (t!=0 & nf) => ~nodetype[1, NONE]
                        solver.addClause(
                            transition[c, k, C + 1],
                            -notFired[c, u, k],
                            -nodeType[c, k, 1, NodeType.NONE.value]
                        )
                        // ff => ~nodetype[1, NONE]
                        solver.declareImply(
                            firstFired[c, u, k],
                            -nodeType[c, k, 1, NodeType.NONE.value]
                        )
                    }


            solver.addComment("CE.+. Equal variables for same inputs in both trees")
            for (input in counterExampleTree.uniqueInputs.intersect(scenarioTree.uniqueInputs)) {
                val uCE = counterExampleTree.uniqueInputs.indexOf(input) + 1
                val uST = scenarioTree.uniqueInputs.indexOf(input) + 1

                for (c in 1..C) {
                    for (j in 1..C) {
                        for (e in 1..E)
                            solver.declareIff(
                                activeTransition[c, j, e, uCE],
                                baseReduction.activeTransition[c, j, e, uST]
                            )
                    }
                    for (k in 1..K) {
                        for (p in 1..P) {
                            solver.declareIff(
                                nodeValue[c, k, p, uCE],
                                baseReduction.nodeValue[c, k, p, uST]
                            )
                            solver.declareIff(
                                childValueLeft[c, k, p, uCE],
                                baseReduction.childValueLeft[c, k, p, uST]
                            )
                            solver.declareIff(
                                childValueRight[c, k, p, uCE],
                                baseReduction.childValueRight[c, k, p, uST]
                            )
                        }
                        solver.declareIff(
                            firstFired[c, uCE, k],
                            baseReduction.firstFired[c, uST, k]
                        )
                        solver.declareIff(
                            notFired[c, uCE, k],
                            baseReduction.notFired[c, uST, k]
                        )
                    }
                }
            }

//            solver.addComment("AaAAAaaAaaaaAAaaARGH!")
//            solver.addClause(-satisfaction[38, C+1])
//            solver.addClause(-satisfaction[18, C+1])

//            solver.addComment("CE. Ad-hoc reverse-implication")
//            // ff[k] <=> root_value[k] & nf[k-1]
//            for (c in 1..C)
//                for (input in counterExampleTree.uniqueInputs - scenarioTree.uniqueInputs) {
//                    val u = counterExampleTree.uniqueInputs.indexOf(input) + 1
//                    for (k in 2..K)
//                        solver.declareIffAnd(firstFired[c, u, k], nodeValue[c, k, 1, u], notFired[c, u, k - 1])
//                }

//            solver.addComment("CE. Ad-hoc 2 reverse-implication")
//            // ff[1] <=> root_value[1]
//            for (c in 1..C)
//                for (input in counterExampleTree.uniqueInputs - scenarioTree.uniqueInputs) {
//                    val u = counterExampleTree.uniqueInputs.indexOf(input) + 1
//                    solver.declareIff(firstFired[c, u, 1], nodeValue[c, 1, 1, u])
//                }

            solver.addComment("CE. Ad-hoc 3 reverse-implication")
            // ff[k] <=> root_value[k] & AND_{k'<k}( ~ff[k'] )
            for (c in 1..C)
                for (input in counterExampleTree.uniqueInputs - scenarioTree.uniqueInputs) {
                    val u = counterExampleTree.uniqueInputs.indexOf(input) + 1
                    for (k in 1..K)
                        solver.declareIffAnd(firstFired[c, u, k], sequence {
                            yield(nodeValue[c, k, 1, u])
                            for (k_ in 1 until k)
                                yield(-firstFired[c, u, k_])
                        })
                }

            return CEReduction(
                counterExampleTree, C, K, P,
                activeTransition,
                nodeValue, childValueLeft, childValueRight, firstFired, notFired,
                satisfaction
            )
        }
    }
}

internal class CEReduction(
    val ceTree: CounterExampleTree,
    val C: Int,
    val K: Int,
    val P: Int,
    val activeTransition: IntMultiArray,
    val nodeValue: IntMultiArray,
    val childValueLeft: IntMultiArray,
    val childValueRight: IntMultiArray,
    val firstFired: IntMultiArray,
    val notFired: IntMultiArray,
    val satisfaction: IntMultiArray
)
