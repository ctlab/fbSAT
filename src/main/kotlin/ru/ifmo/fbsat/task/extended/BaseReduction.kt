package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.automaton.NodeType
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.solver.exactlyOne
import ru.ifmo.fbsat.solver.iff
import ru.ifmo.fbsat.solver.iffAnd
import ru.ifmo.fbsat.solver.iffOr
import ru.ifmo.fbsat.solver.imply
import ru.ifmo.fbsat.solver.implyIff
import ru.ifmo.fbsat.solver.implyIffAnd
import ru.ifmo.fbsat.solver.implyIffOr
import ru.ifmo.fbsat.utils.IntMultiArray

class BaseReduction(
    val scenarioTree: ScenarioTree,
    val C: Int,
    val K: Int,
    val P: Int,
    solver: Solver
) {
    // Constants
    private val V = scenarioTree.size
    private val E = scenarioTree.inputEvents.size
    private val O = scenarioTree.outputEvents.size
    private val U = scenarioTree.uniqueInputs.size
    private val X = scenarioTree.uniqueInputs.first().length
    private val Z = scenarioTree.uniqueOutputs.first().length
    // Scenario tree variables
    val color: IntMultiArray
    // Automaton variables
    val transition: IntMultiArray
    val actualTransition: IntMultiArray
    val inputEvent: IntMultiArray
    val outputEvent: IntMultiArray
    val algorithm0: IntMultiArray
    val algorithm1: IntMultiArray
    // Guards variables
    val nodeType: IntMultiArray
    val terminal: IntMultiArray
    val parent: IntMultiArray
    val childLeft: IntMultiArray
    val childRight: IntMultiArray
    val nodeValue: IntMultiArray
    val rootValue: IntMultiArray
    val childValueLeft: IntMultiArray
    val childValueRight: IntMultiArray
    val firstFired: IntMultiArray
    val notFired: IntMultiArray
    // BFS variables
    val bfsTransition: IntMultiArray
    val bfsParent: IntMultiArray

    init {
        with(solver) {
            // Scenario tree variables
            color = newArray(V, C)
            // Automaton variables
            transition = newArray(C, K, C + 1)
            actualTransition = newArray(C, E, U, C + 1)
            inputEvent = newArray(C, K, E + 1)
            outputEvent = newArray(C, O)
            algorithm0 = newArray(C, Z)
            algorithm1 = newArray(C, Z)
            // Guards variables
            nodeType = newArray(C, K, P, 5)
            terminal = newArray(C, K, P, X + 1)
            parent = newArray(C, K, P, P + 1)
            childLeft = newArray(C, K, P, P + 1)
            childRight = newArray(C, K, P, P + 1)
            nodeValue = newArray(C, K, P, U)
            rootValue = newArray(C, K, U) { (c, k, u) -> nodeValue[c, k, 1, u] }
            childValueLeft = newArray(C, K, P, U)
            childValueRight = newArray(C, K, P, U)
            firstFired = newArray(C, U, K + 1)
            notFired = newArray(C, U, K)
            // BFS variables
            bfsTransition = newArray(C, C)
            bfsParent = newArray(C, C)

            // Constraints
            declareColorConstraints()
            declareTransitionConstraints()
            declareFiringConstraints()
            declareOutputEventConstraints()
            declareAlgorithmConstraints()
            declareBfsConstraints()
            declareNodeTypeConstraints()
            declareParentAndChildrenConstraints()
            declareNoneTypeNodesConstraints()
            declareTerminalsConstraits()
            declareAndOrNodesConstraints()
            declareNotNodesConstraints()
            declareAdhocConstraints()
        }
    }

    private fun Solver.declareColorConstraints() {
        comment("1. Color constraints")

        comment("1.0. ONE(color)_{1..C}")
        for (v in 1..V)
            exactlyOne(1..C, color, v)

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

    private fun Solver.declareTransitionConstraints() {
        comment("2. Transition constraints")

        comment("2.0a. ONE(transition)_{0..C}")
        for (c in 1..C)
            for (k in 1..K)
                exactlyOne(1..(C + 1), transition, c, k)

        comment("2.0b. ONE(actual_transition)_{0..C}")
        for (c in 1..C)
            for (e in 1..E)
                for (u in 1..U)
                    exactlyOne(1..(C + 1), actualTransition, c, e, u)

        comment("2.0c. ONE(input_event)_{0..E}")
        for (c in 1..C)
            for (k in 1..K)
                exactlyOne(1..(E + 1), inputEvent, c, k)

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
            for (k in 1..(K - 1))
                imply(transition[c, k, C + 1], transition[c, k + 1, C + 1])

        comment("2.3. Only null-transitions have no input event")
        // transition[k, 0] <=> input_event[k, 0]
        for (c in 1..C)
            for (k in 1..K)
                iff(transition[c, k, C + 1], inputEvent[c, k, E + 1])

        comment("+2.4. Ad-hoc: no transition to the first state")
        for (c in 1..C)
            clause(-transition[c, 1])
    }

    private fun Solver.declareFiringConstraints() {
        comment("3. Firing constraints")

        comment("3.0. ONE(first_fired)_{0..K}")
        for (c in 1..C)
            for (u in 1..U)
                exactlyOne(1..(K + 1), firstFired, c, u)

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
                for (k in 1..(K - 1))
                    imply(-notFired[c, u, k], -notFired[c, u, k + 1])

        comment("3.4. first_fired[0] <=> not_fired[K] (shortcut)")
        // first_fired[0] <=> not_fired[K]
        for (c in 1..C)
            for (u in 1..U)
                iff(firstFired[c, u, K + 1], notFired[c, u, K])
    }

    private fun Solver.declareOutputEventConstraints() {
        comment("4. Output event constraints")

        comment("4.0. ONE(output_event)_{1..O}")
        for (c in 1..C)
            exactlyOne(1..O, outputEvent, c)

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

    private fun Solver.declareAlgorithmConstraints() {
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
                            else -> error("Weird combination of values: $values")
                        }
                    )
            }
    }

    private fun Solver.declareBfsConstraints() {
        comment("6. BFS constraints")

        comment("6.1. F_t")
        // t[i, j] <=> OR_k( transition[i,k,j] )
        for (i in 1..C)
            for (j in 1..C)
                iffOr(bfsTransition[i, j], sequence {
                    for (k in 1..K)
                        yield(transition[i, k, j])
                })

        comment("6.2. F_p")
        // p[j, i] <=> t[i, j] & AND_{k<i}( ~t[k, j] )
        for (i in 1..C) {
            // to avoid ambiguous unused variable:
            for (j in 1..i)
                clause(-bfsParent[j, i])

            for (j in (i + 1)..C)
                iffAnd(bfsParent[j, i], sequence {
                    yield(bfsTransition[i, j])
                    for (k in 1..(i - 1))
                        yield(-bfsTransition[k, j])
                })
        }

        comment("6.3. F_ALO(p)")
        for (j in 2..C)
            clause {
                for (i in 1..(j - 1))
                    yield(bfsParent[j, i])
            }

        comment("6.4. F_BFS(p)")
        // p[j, i] => ~p[j+1, k]
        for (k in 1..C)
            for (i in (k + 1)..C)
                for (j in (i + 1)..(C - 1))
                    imply(bfsParent[j, i], -bfsParent[j + 1, k])
    }

    private fun Solver.declareNodeTypeConstraints() {
        comment("7. Nodetype constraints")

        comment("7.0. ONE(nodetype)_{1..5}")
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..P)
                    exactlyOne(1..5, nodeType, c, k, p)

        comment("7.1. Only null-transitions have no guard")
        // transition[0] <=> nodetype[1, NONE]
        for (c in 1..C)
            for (k in 1..K)
                iff(transition[c, k, C + 1], nodeType[c, k, 1, NodeType.NONE.value])
    }

    private fun Solver.declareParentAndChildrenConstraints() {
        comment("8. Parent and children constraints")

        comment("8.0a. ONE(parent)_{0..P}")
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..P)
                    exactlyOne(1..(P + 1), parent, c, k, p)

        comment("8.0b. ONE(child_left)_{0..P}")
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..P)
                    exactlyOne(1..(P + 1), childLeft, c, k, p)

        comment("8.0c. ONE(child_right)_{0..P}")
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
    }

    private fun Solver.declareNoneTypeNodesConstraints() {
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
    }

    private fun Solver.declareTerminalsConstraits() {
        comment("10. Terminals constraints")

        comment("10.0. ONE(terminal)_{0..X}")
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
                                else -> error("Character $char for u = $u, x = $x is neither '1' nor '0'")
                            }
    }

    private fun Solver.declareAndOrNodesConstraints() {
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
    }

    private fun Solver.declareNotNodesConstraints() {
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
                    for (u in 1..U)
                        implyIff(
                            nodeType[c, k, p, NodeType.NOT.value],
                            nodeValue[c, k, p, u],
                            -childValueLeft[c, k, p, u]
                        )
    }

    private fun Solver.declareAdhocConstraints() {
        comment("A. AD-HOCs")

        comment("A.1. Forbid double negation")
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

        comment("A.2. Distinct transitions")
        // TODO: Distinct transitions
    }
}
