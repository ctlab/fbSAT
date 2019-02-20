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
    private val oldV = previousNegativeReduction?.V ?: 0
    private val V = negativeScenarioTree.size
    private val E = negativeScenarioTree.inputEvents.size
    private val U = negativeScenarioTree.uniqueInputs.size
    private val X = negativeScenarioTree.uniqueInputs.first().length
    private val Z = negativeScenarioTree.uniqueOutputs.first().length
    private val newU = (1..U).filter { u ->
        negativeScenarioTree.uniqueInputs[u - 1] !in scenarioTree.uniqueInputs
    }
    // Negative scenario tree variables
    val satisfaction: IntMultiArray
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
    val childLeft: IntMultiArray
    val childRight: IntMultiArray
    val nodeValue: IntMultiArray
    val rootValue: IntMultiArray
    val childValueLeft: IntMultiArray
    val childValueRight: IntMultiArray
    val firstFired: IntMultiArray
    val notFired: IntMultiArray

    init {
        println("[.] C = $C")
        println("[.] K = $K")
        println("[.] P = $P")
        println("[.] oldV = $oldV")
        println("[.] V = $V")
        println("[.] E = $E")
        println("[.] U = $U")
        println("[.] X = $X")
        println("[.] Z = $Z")
        println("[.] newU = $newU")
        check(newU.size == (negativeScenarioTree.uniqueInputs - scenarioTree.uniqueInputs).size)
    }

    init {
        if (previousNegativeReduction != null)
            TODO("Incremental negative reduction is not implemented yet")

        fun isomorphicInputNumber(u: Int): Int? =
            when (val uuu = scenarioTree.uniqueInputs.indexOf(negativeScenarioTree.uniqueInputs[u - 1])) {
                -1 -> null
                else -> uuu + 1
            }

        with(solver) {
            // Negative scenario tree variables
            satisfaction = newArray(V, C + 1) { (v, c) ->
                if (v <= oldV)
                    previousNegativeReduction!!.satisfaction[v, c]
                else
                    newVariable()
            }
            // Automaton variables
            transition = baseReduction.transition
            actualTransition = newArray(C, E, U, C + 1) { (i, e, u, j) ->
                isomorphicInputNumber(u)?.let { u_ -> baseReduction.actualTransition[i, e, u_, j] } ?: newVariable()
            }
            inputEvent = baseReduction.inputEvent
            outputEvent = baseReduction.outputEvent
            algorithm0 = baseReduction.algorithm0
            algorithm1 = baseReduction.algorithm1
            // Guards variables
            nodeType = baseReduction.nodeType
            terminal = baseReduction.terminal
            childLeft = baseReduction.childLeft
            childRight = baseReduction.childRight
            nodeValue = newArray(C, K, P, U) { (c, k, p, u) ->
                isomorphicInputNumber(u)?.let { u_ -> baseReduction.nodeValue[c, k, p, u_] } ?: newVariable()
            }
            rootValue = newArray(C, K, U) { (c, k, u) ->
                nodeValue[c, k, 1, u]
            }
            childValueLeft = newArray(C, K, P, U) { (c, k, p, u) ->
                isomorphicInputNumber(u)?.let { u_ -> baseReduction.childValueLeft[c, k, p, u_] } ?: newVariable()
            }
            childValueRight = newArray(C, K, P, U) { (c, k, p, u) ->
                isomorphicInputNumber(u)?.let { u_ -> baseReduction.childValueRight[c, k, p, u_] } ?: newVariable()
            }
            firstFired = newArray(C, U, K + 1) { (c, u, k) ->
                isomorphicInputNumber(u)?.let { u_ -> baseReduction.firstFired[c, u_, k] } ?: newVariable()
            }
            notFired = newArray(C, U, K) { (c, u, k) ->
                isomorphicInputNumber(u)?.let { u_ -> baseReduction.notFired[c, u_, k] } ?: newVariable()
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
        for (v in 1..V) {
            if (v <= oldV) continue
            exactlyOne(1..(C + 1), satisfaction, v)
        }

        comment("CE.1.1. Satisfaction of active vertices")
        // satisfaction[tp(v), i] & actual_transition[i,tie(v),tin(v),j] => satisfaction[v, j]
        for (v in negativeScenarioTree.activeVertices) {
            if (v <= oldV) continue
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
        for (v in negativeScenarioTree.activeVertices) {
            continue
            if (v <= oldV) continue
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
        for (v in negativeScenarioTree.passiveVertices) {
            if (v <= oldV) continue
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
        for (v in negativeScenarioTree.passiveVertices) {
            continue
            if (v <= oldV) continue
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
        for (v in negativeScenarioTree.passiveVertices) {
            if (v <= oldV) continue
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
        for (v in 2..V) {
            if (v <= oldV) continue
            val p = negativeScenarioTree.parent(v)
            imply(satisfaction[p, C + 1], satisfaction[v, C + 1])
        }

        comment("CE.1.5. Forbid loops")
        // satisfaction[v, c] => ~satisfaction[loop(v), c]
        if (isForbidLoops)
            for (v in negativeScenarioTree.verticesWithLoops) {
                if (v <= oldV) continue
                for (l in negativeScenarioTree.loopBacks(v))
                    for (c in 1..C)
                        imply(satisfaction[v, c], -satisfaction[l, c])
            }
        else
            comment("===== NOT FORBIDDING LOOPS =====")

        comment("CE.1.6. Root is satisfied by start state")
        if (oldV == 0)
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
        for (v in negativeScenarioTree.activeVertices) {
            if (v <= oldV) continue
            val o = negativeScenarioTree.outputEvent(v)
            for (c in 1..C)
                imply(satisfaction[v, c], outputEvent[c, o])
        }
    }

    private fun Solver.declareAlgorithmConstraints() {
        comment("CE.5. Algorithm constraints")

        comment("CE.5.2. Algorithms definition")
        for (v in negativeScenarioTree.activeVertices) {
            if (v <= oldV) continue
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
                            when (val char = negativeScenarioTree.uniqueInputs[u - 1][x - 1]) {
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
