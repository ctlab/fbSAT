package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.automaton.NodeType
import ru.ifmo.fbsat.scenario.NegativeScenarioTree
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

internal class NegativeReduction(
    val scenarioTree: ScenarioTree,
    val baseReduction: BaseReduction,
    val negativeScenarioTree: NegativeScenarioTree,
    val previousNegativeReduction: NegativeReduction?,
    solver: Solver,
    private val isForbidLoops: Boolean = true
) {
    // Constants
    val C = baseReduction.C
    val K = baseReduction.K
    val P = baseReduction.P
    private val V = negativeScenarioTree.size
    private val oldV = previousNegativeReduction?.V ?: 0
    private val Vs = (oldV + 1)..V
    private val VsWithoutRoot = Vs.filter { it != 1 }
    private val VsWithLoops = Vs.filter { it in negativeScenarioTree.verticesWithLoops }
    private val activeVs = Vs.filter { it in negativeScenarioTree.activeVertices }
    private val passiveVs = Vs.filter { it in negativeScenarioTree.passiveVertices }
    private val E = negativeScenarioTree.inputEvents.size
    private val UIs = negativeScenarioTree.uniqueInputs
    private val posUIs = scenarioTree.uniqueInputs
    private val negUIs = UIs - posUIs
    private val oldNegUIs = previousNegativeReduction?.negUIs ?: emptyList()
    private val newNegUIs = negUIs - oldNegUIs
    private val U = UIs.size
    private val newU = newNegUIs.map { negativeScenarioTree.uniqueInputNumber(it) }
    private val X = negativeScenarioTree.uniqueInputs.first().length
    private val Z = negativeScenarioTree.uniqueOutputs.first().length
    // Negative scenario tree variables
    val satisfaction: IntMultiArray // [V, C+1]
    // Automaton variables
    val transition: IntMultiArray // [C, K, C+1]
    val actualTransition: IntMultiArray // [C, E, U, C+1]
    val inputEvent: IntMultiArray // [C, K, E]
    val outputEvent: IntMultiArray // [C, O]
    val algorithm0: IntMultiArray // [C, Z]
    val algorithm1: IntMultiArray // [C, Z]
    // Guards variables
    val nodeType: IntMultiArray // [C, K, P, 5]
    val terminal: IntMultiArray // [C, K, P, X+1]
    val childLeft: IntMultiArray // [C, K, P, P+1]
    val childRight: IntMultiArray // [C, K, P, P+1]
    val nodeValue: IntMultiArray // [C, K, P, U]
    val rootValue: IntMultiArray // [C, K, U]
    val childValueLeft: IntMultiArray // [C, K, P, U]
    val childValueRight: IntMultiArray // [C, K, P, U]
    val firstFired: IntMultiArray // [C, U, K+1]
    val notFired: IntMultiArray // [C, U, K]

    init {
        println("[.] C = $C, K = $K, P = $P, V = $V, oldV = $oldV, Vs = $Vs, E = $E, U = $U, newU = $newU, X = $X, Z = $Z")
    }

    init {
        fun getPosU(input: String): Int = posUIs.indexOf(input) + 1
        fun getOldNegU(input: String): Int = oldNegUIs.indexOf(input) + 1

        with(solver) {
            // Negative scenario tree variables
            satisfaction =
                IntMultiArray.new(V, C + 1) { (v, c) ->
                    if (v in Vs)
                        newVariable()
                    else
                        previousNegativeReduction!!.satisfaction[v, c]
                }
            // Automaton variables
            transition = baseReduction.transition
            actualTransition = IntMultiArray.new(C, E, U, C + 1) { (i, e, u, j) ->
                when (val input = UIs[u - 1]) {
                    in newNegUIs -> newVariable()
                    in oldNegUIs -> previousNegativeReduction!!.actualTransition[i, e, getOldNegU(input), j]
                    else -> baseReduction.actualTransition[i, e, getPosU(input), j]
                }
            }
            //     isomorphicInputNumber(u)?.let { u_ -> baseReduction.actualTransition[i, e, u_, j] } ?: newVariable()
            // }
            inputEvent = baseReduction.inputEvent
            outputEvent = baseReduction.outputEvent
            algorithm0 = baseReduction.algorithm0
            algorithm1 = baseReduction.algorithm1
            // Guards variables
            nodeType = baseReduction.nodeType
            terminal = baseReduction.terminal
            childLeft = baseReduction.childLeft
            childRight = baseReduction.childRight
            nodeValue = IntMultiArray.new(C, K, P, U) { (c, k, p, u) ->
                when (val input = UIs[u - 1]) {
                    in newNegUIs -> newVariable()
                    in oldNegUIs -> previousNegativeReduction!!.nodeValue[c, k, p, getOldNegU(input)]
                    else -> baseReduction.nodeValue[c, k, p, getPosU(input)]
                }
            }
            rootValue = IntMultiArray.new(C, K, U) { (c, k, u) ->
                nodeValue[c, k, 1, u]
            }
            childValueLeft = IntMultiArray.new(C, K, P, U) { (c, k, p, u) ->
                when (val input = UIs[u - 1]) {
                    in newNegUIs -> newVariable()
                    in oldNegUIs -> previousNegativeReduction!!.childValueLeft[c, k, p, getOldNegU(input)]
                    else -> baseReduction.childValueLeft[c, k, p, getPosU(input)]
                }
            }
            childValueRight = IntMultiArray.new(C, K, P, U) { (c, k, p, u) ->
                when (val input = UIs[u - 1]) {
                    in newNegUIs -> newVariable()
                    in oldNegUIs -> previousNegativeReduction!!.childValueRight[c, k, p, getOldNegU(input)]
                    else -> baseReduction.childValueRight[c, k, p, getPosU(input)]
                }
            }
            firstFired = IntMultiArray.new(C, U, K + 1) { (c, u, k) ->
                when (val input = UIs[u - 1]) {
                    in newNegUIs -> newVariable()
                    in oldNegUIs -> previousNegativeReduction!!.firstFired[c, getOldNegU(input), k]
                    else -> baseReduction.firstFired[c, getPosU(input), k]
                }
            }
            notFired = IntMultiArray.new(C, U, K) { (c, u, k) ->
                when (val input = UIs[u - 1]) {
                    in newNegUIs -> newVariable()
                    in oldNegUIs -> previousNegativeReduction!!.notFired[c, getOldNegU(input), k]
                    else -> baseReduction.notFired[c, getPosU(input), k]
                }
            }

            // Constraints
            declareSatisfactionConstraints()
            declareTransitionConstraints()
            declareFiringConstraints()
            declareOutputEventConstraints()
            declareAlgorithmConstraints()
            declareGuardConstraints()
        }
    }

    private fun Solver.declareSatisfactionConstraints() {
        comment("CE.1. Satisfaction (color-like) constrains")

        comment("CE.1.0. ONE(satisfaction)_{0..C}")
        for (v in Vs)
            exactlyOne(1..(C + 1), satisfaction, v)

        comment("CE.1.1. Satisfaction of active vertices")
        // satisfaction[tp(v), i] & actual_transition[i,tie(v),tin(v),j] => satisfaction[v, j]
        for (v in activeVs) {
            val p = negativeScenarioTree.parent(v)
            val e = negativeScenarioTree.inputEvent(v)
            val u = negativeScenarioTree.inputNumber(v)
            for (i in 1..C)
                for (j in 1..C)
                    clause(
                        -satisfaction[p, i],
                        -actualTransition[i, e, u, j],
                        satisfaction[v, j]
                    )
        }

        comment("CE.1.1+. Non-satisfaction of active vertices (redundant)")
        // satisfaction[tp(v), c] & actual_transition[c,tie(v),tin(v),0] => satisfaction[v, 0]
        for (v in activeVs) {
            continue
            val p = negativeScenarioTree.parent(v)
            val e = negativeScenarioTree.inputEvent(v)
            val u = negativeScenarioTree.inputNumber(v)
            for (c in 1..C)
                clause(
                    -satisfaction[p, c],
                    -actualTransition[c, e, u, C + 1],
                    satisfaction[v, C + 1]
                )
        }

        comment("CE.1.2. Satisfaction of passive vertices")
        // satisfaction[tp(v), c] & actual_transition[c,tie(v),tin(v),0] => satisfaction[v, c]
        for (v in passiveVs) {
            val p = negativeScenarioTree.parent(v)
            val e = negativeScenarioTree.inputEvent(v)
            val u = negativeScenarioTree.inputNumber(v)
            for (c in 1..C)
                clause(
                    -satisfaction[p, c],
                    -actualTransition[c, e, u, C + 1],
                    satisfaction[v, c]
                )
        }

        comment("CE.1.2+. Non-satisfaction of passive vertices (redundant)")
        // satisfaction[tp(v), c] & ~actual_transition[c,tie(v),tin(v),0] => satisfaction[v, 0]
        for (v in passiveVs) {
            continue
            val p = negativeScenarioTree.parent(v)
            val e = negativeScenarioTree.inputEvent(v)
            val u = negativeScenarioTree.inputNumber(v)
            for (i in 1..C)
                for (j in 1..C)
                    clause(
                        -satisfaction[p, i],
                        actualTransition[i, e, u, C + 1],
                        satisfaction[v, C + 1]
                    )
        }

        comment("CE.1.3. Propagation of satisfaction for passive vertices")
        // satisfaction[tp(v), c] => satisfaction[v, c] | satisfaction[v, 0]
        for (v in passiveVs) {
            val p = negativeScenarioTree.parent(v)
            for (c in 1..C)
                clause(
                    -satisfaction[p, c],
                    satisfaction[v, c],
                    satisfaction[v, C + 1]
                )
        }

        comment("CE.1.4. Propagation of non-satisfaction")
        // satisfaction[tp(v), 0] => satisfaction[v, 0]
        for (v in VsWithoutRoot) {
            val p = negativeScenarioTree.parent(v)
            imply(satisfaction[p, C + 1], satisfaction[v, C + 1])
        }

        comment("CE.1.5. Forbid loops")
        // satisfaction[v, c] => ~satisfaction[loop(v), c]
        if (isForbidLoops)
            for (v in VsWithLoops) {
                for (l in negativeScenarioTree.loopBacks(v))
                    for (c in 1..C)
                        imply(satisfaction[v, c], -satisfaction[l, c])
            }
        else
            comment("===== NOT FORBIDDING LOOPS =====")

        comment("CE.1.6. Root is satisfied by start state")
        if (1 in Vs)
            clause(satisfaction[1, 1])
    }

    private fun Solver.declareTransitionConstraints() {
        comment("CE.2. Transition constraints")

        comment("CE.2.0b. ONE(actual_transition)_{0..C}")
        for (c in 1..C)
            for (e in 1..E)
                for (u in newU)
                    exactlyOne(1..(C + 1), actualTransition, c, e, u)

        comment("CE.2.1. Active transition definition")
        // actual_transition[i,e,u,j] <=> OR_k( transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k] )
        for (i in 1..C)
            for (e in 1..E)
                for (u in newU)
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
    }

    private fun Solver.declareFiringConstraints() {
        comment("CE.3. Firing constraints")

        comment("CE.3.0. ONE(first_fired)_{0..K}")
        for (c in 1..C)
            for (u in newU)
                exactlyOne(1..(K + 1), firstFired, c, u)

        comment("CE.3.1. first_fired definition")
        // first_fired[k] <=> root_value[k] & not_fired[k-1]
        for (c in 1..C)
            for (u in newU) {
                iff(firstFired[c, u, 1], rootValue[c, 1, u])
                for (k in 2..K)
                    iffAnd(firstFired[c, u, k], rootValue[c, k, u], notFired[c, u, k - 1])
            }

        comment("CE.3.2. not_fired definition")
        // not_fired[k] <=> ~root_value[k] & not_fired[k-1]
        for (c in 1..C)
            for (u in newU) {
                iff(notFired[c, u, 1], -rootValue[c, 1, u])
                for (k in 2..K)
                    iffAnd(notFired[c, u, k], -rootValue[c, k, u], notFired[c, u, k - 1])
            }

        comment("CE.3.3. Propagation of not-not_fired (maybe redundant)")
        // ~not_fired[k] => ~not_fired[k+1]
        for (c in 1..C)
            for (u in newU)
                for (k in 1..(K - 1))
                    imply(-notFired[c, u, k], -notFired[c, u, k + 1])

        comment("CE.3.4. first_fired[0] <=> not_fired[K] (shortcut)")
        // first_fired[0] <=> not_fired[K]
        for (c in 1..C)
            for (u in newU)
                iff(firstFired[c, u, K + 1], notFired[c, u, K])
    }

    private fun Solver.declareOutputEventConstraints() {
        comment("CE.4. Output event constraints")

        comment("CE.4.1. output_event definition")
        // satisfaction[v, c] => output_event[c, toe(v)]
        for (v in activeVs) {
            val o = negativeScenarioTree.outputEvent(v)
            for (c in 1..C)
                imply(satisfaction[v, c], outputEvent[c, o])
        }
    }

    private fun Solver.declareAlgorithmConstraints() {
        comment("CE.5. Algorithm constraints")

        comment("CE.5.2. Algorithms definition")
        for (v in activeVs)
            for (z in 1..Z) {
                val oldValue = negativeScenarioTree.outputValue(negativeScenarioTree.parent(v), z)
                val newValue = negativeScenarioTree.outputValue(v, z)
                for (c in 1..C)
                    imply(
                        satisfaction[v, c],
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

    private fun Solver.declareGuardConstraints() {
        comment("CE. Guard constraints re-definition for CE unique inputs")

        comment("CE.9.3. None-type nodes have False value and child_values")
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..P)
                    for (u in newU) {
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
                    for (u in newU) {
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
                        for (u in newU)
                            when (val char = UIs[u - 1][x - 1]) {
                                '1' -> imply(terminal[c, k, p, x], nodeValue[c, k, p, u])
                                '0' -> imply(terminal[c, k, p, x], -nodeValue[c, k, p, u])
                                else -> error("Character $char for u = $u, x = $x is neither '1' nor '0'")
                            }

        comment("CE.11.4a. AND/OR: child_value_left is a value of left child")
        // nodetype[p, AND/OR] & child_left[p, ch] => AND_u( child_value_left[p,u] <=> value[ch, u] )
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..(P - 2))
                    for (ch in (p + 1)..(P - 1))
                        for (u in newU)
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
                        for (u in newU)
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
                    for (u in newU)
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
                    for (u in newU)
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
                        for (u in newU) {
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
                    for (u in newU)
                        imply(
                            nodeType[c, k, p, NodeType.NOT.value],
                            -childValueRight[c, k, p, u]
                        )

        comment("CE.12.5. NOT: value is calculated as a negation of child")
        // nodetype[p, NOT] => AND_u( value[p, u] <=> ~child_value_left[p, u] )
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..(P - 1))
                    for (u in newU)
                        implyIff(
                            nodeType[c, k, p, NodeType.NOT.value],
                            nodeValue[c, k, p, u],
                            -childValueLeft[c, k, p, u]
                        )
    }
}
