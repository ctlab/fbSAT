package ru.ifmo.fbsat.core.constraints

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.ScenarioTreeInterface
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.iff
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.implyImply
import ru.ifmo.fbsat.core.solver.implyImplyIff
import ru.ifmo.fbsat.core.solver.implyImplyIffAnd
import ru.ifmo.fbsat.core.solver.implyImplyIffOr
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.ConsecutiveModularExtendedVariables
import ru.ifmo.fbsat.core.task.single.complete.CompleteVariables
import ru.ifmo.fbsat.core.task.single.extended.ExtendedVariables
import ru.ifmo.fbsat.core.utils.boolToSign

fun Solver.declarePositiveGuardConditionsConstraints(extendedVars: ExtendedVariables) {
    comment("Positive guard conditions constraints")
    with(extendedVars) {
        comment("Positive guard conditions constraints: inputless")
        declareGuardConditionsConstraintsInputless(
            C = C, K = K, P = P, X = X,
            transitionDestination = transitionDestination,
            nodeType = nodeType,
            nodeInputVariable = nodeInputVariable,
            nodeParent = nodeParent,
            nodeChild = nodeChild
        )

        for (u in 1..U) {
            comment("Positive guard conditions constraints: for input u = $u")
            declareGuardConditionsConstraintsForInput(
                u = u,
                tree = scenarioTree,
                C = C, K = K, P = P, X = X,
                nodeType = nodeType,
                nodeInputVariable = nodeInputVariable,
                nodeChild = nodeChild,
                nodeValue = nodeValue
            )
        }
    }
}

fun Solver.declareNegativeGuardConditionsConstraints(
    completeVars: CompleteVariables,
    Us: Iterable<Int>
) {
    comment("Negative guard conditions constraints")
    with(completeVars) {
        // Note: no inputless constraints

        // Note: be very careful with positive/negative variables!
        for (u in Us) {
            comment("Negative guard conditions constraints: for input u = $u")
            declareGuardConditionsConstraintsForInput(
                u = u,
                tree = negativeScenarioTree,
                C = C, K = K, P = P, X = X,
                nodeType = nodeType,
                nodeInputVariable = nodeInputVariable,
                nodeChild = nodeChild,
                nodeValue = negNodeValue
            )
        }
    }
}

fun Solver.declareConsecutiveModularGuardConditionsConstraints(
    consecutiveModularExtendedVariables: ConsecutiveModularExtendedVariables
) {
    println("Consecutive modular guard conditions constraints")
    with(consecutiveModularExtendedVariables) {
        for (m in 1..M) {
            println("Consecutive modular guard conditions constraints: for module m = $m")
            declarePositiveGuardConditionsConstraints(
                modularExtendedVariables[m]
            )
        }
    }
}

private fun Solver.declareGuardConditionsConstraintsInputless(
    C: Int,
    K: Int,
    P: Int,
    X: Int,
    transitionDestination: IntMultiArray,
    nodeType: IntMultiArray,
    nodeInputVariable: IntMultiArray,
    nodeParent: IntMultiArray,
    nodeChild: IntMultiArray
) {
    comment("None-typed nodes have largest indices")
    // (nodeType[p] = NONE) => (nodeType[p+1] = NONE)
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1 until P)
                imply(
                    nodeType[c, k, p, NodeType.NONE.value],
                    nodeType[c, k, p + 1, NodeType.NONE.value]
                )

    comment("Only null-transitions have no guard (root is none-typed)")
    // (transitionDestination = 0) <=> (nodeType[1] = NONE)
    for (c in 1..C)
        for (k in 1..K)
            iff(
                transitionDestination[c, k, C + 1],
                nodeType[c, k, 1, NodeType.NONE.value]
            )

    comment("child=>parent relation")
    // (nodeChild[p] = ch) => (nodeParent[ch] = p)
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (ch in (p + 1)..P)
                    imply(
                        nodeChild[c, k, p, ch],
                        nodeParent[c, k, ch, p]
                    )

    comment("Only typed nodes, except the root, have parents")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 2..P)
                iff(
                    -nodeParent[c, k, p, P + 1],
                    -nodeType[c, k, p, NodeType.NONE.value]
                )

    // TERMINALS CONSTRAINTS

    comment("Only terminal nodes have associated input variables")
    // (nodeType[p] = TERMINAL) <=> (nodeInputVariable[p] != 0)
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                iff(
                    nodeType[c, k, p, NodeType.TERMINAL.value],
                    -nodeInputVariable[c, k, p, X + 1]
                )

    comment("Terminals do not have children")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                imply(
                    nodeType[c, k, p, NodeType.TERMINAL.value],
                    nodeChild[c, k, p, P + 1]
                )

    // AND/OR NODES CONSTRAINTS

    comment("11.0a. AND/OR nodes cannot have numbers P-1 or P")
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

    comment("11.0b. AND/OR: left child cannot have number P")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (nt in listOf(NodeType.AND, NodeType.OR))
                    imply(
                        nodeType[c, k, p, nt.value],
                        -nodeChild[c, k, p, P]
                    )

    comment("11.1. AND/OR nodes have left child")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (t in listOf(NodeType.AND, NodeType.OR))
                    imply(
                        nodeType[c, k, p, t.value],
                        -nodeChild[c, k, p, P + 1]
                    )

    comment("11.3. AND/OR: hard to explain")
    // parent[p, par] & nodetype[par, AND/OR] => child[par, p] | child[par, p-1]
    for (c in 1..C)
        for (k in 1..K)
            for (par in 1..P)
                for (ch in (par + 1) until P)
                    for (nt in listOf(NodeType.AND, NodeType.OR))
                        clause(
                            -nodeType[c, k, par, nt.value],
                            -nodeParent[c, k, ch, par],
                            nodeChild[c, k, par, ch],
                            nodeChild[c, k, par, ch - 1]
                        )

    comment("Right child of binary operators follows the left one")
    // (nodeType[p] = AND/OR) & (nodeChild[p] = ch) => (nodeParent[ch+1] = p)
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (ch in (p + 1) until P)
                    for (t in listOf(NodeType.AND, NodeType.OR))
                        implyImply(
                            nodeType[c, k, p, t.value],
                            nodeChild[c, k, p, ch],
                            nodeParent[c, k, ch + 1, p]
                        )

    // NOT NODES CONSTRAINTS

    comment("12.0. NOT nodes cannot have number P")
    for (c in 1..C)
        for (k in 1..K)
            if (P >= 1)
                clause(-nodeType[c, k, P, NodeType.NOT.value])

    comment("12.1. NOT nodes have left child")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1 until P)
                imply(
                    nodeType[c, k, p, NodeType.NOT.value],
                    -nodeChild[c, k, p, P + 1]
                )

    comment("12.2. NOT: parent's child is the current node")
    // parent[p, par] & nodetype[par, NOT] => child[par, p]
    for (c in 1..C)
        for (k in 1..K)
            for (par in 1..P)
                for (ch in (par + 1)..P)
                    implyImply(
                        nodeType[c, k, par, NodeType.NOT.value],
                        nodeParent[c, k, ch, par],
                        nodeChild[c, k, par, ch]
                    )

    // NONE-TYPE NODES CONSTRAINTS

    comment("None-typed nodes do not have children")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                imply(
                    nodeType[c, k, p, NodeType.NONE.value],
                    nodeChild[c, k, p, P + 1]
                )
}

private fun Solver.declareGuardConditionsConstraintsForInput(
    u: Int,
    tree: ScenarioTreeInterface,
    C: Int,
    K: Int,
    P: Int,
    X: Int,
    nodeType: IntMultiArray,
    nodeInputVariable: IntMultiArray,
    nodeChild: IntMultiArray,
    nodeValue: IntMultiArray
) {
    comment("Terminal nodes have value from associated input variables")
    // (nodeInputVariable[p] = x) => AND_{u}( nodeValue[p,u] <=> u[x] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (x in 1..X)
                    imply(
                        nodeInputVariable[c, k, p, x],
                        nodeValue[c, k, p, u] * boolToSign(tree.uniqueInputs[u - 1][x - 1])
                    )

    comment("AND: value is calculated as a conjunction of children values")
    // (nodeType[p] = AND) & (nodeChild[p] = ch) =>
    //  AND_{u}( nodeValue[p,u] <=> nodeValue[ch,u] & nodeValue[ch+1,u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (ch in (p + 1) until P)
                    implyImplyIffAnd(
                        nodeType[c, k, p, NodeType.AND.value],
                        nodeChild[c, k, p, ch],
                        nodeValue[c, k, p, u],
                        nodeValue[c, k, ch, u],
                        nodeValue[c, k, ch + 1, u]
                    )

    comment("OR: value is calculated as a disjunction of children values")
    // (nodeType[p] = OR) & (nodeChild[p] = ch) =>
    //  AND_{u}( nodeValue[p,u] <=> nodeValue[ch,u] | nodeValue[ch+1,u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (ch in (p + 1) until P)
                    implyImplyIffOr(
                        nodeType[c, k, p, NodeType.OR.value],
                        nodeChild[c, k, p, ch],
                        nodeValue[c, k, p, u],
                        nodeValue[c, k, ch, u],
                        nodeValue[c, k, ch + 1, u]
                    )

    comment("NOT: value is calculated as a negation of a child value")
    // (nodeType[p] = OR) & (nodeChild[p] = ch) =>
    //  AND_{u}( nodeValue[p,u] <=> ~nodeValue[ch,u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (ch in (p + 1)..P)
                    implyImplyIff(
                        nodeType[c, k, p, NodeType.NOT.value],
                        nodeChild[c, k, p, ch],
                        nodeValue[c, k, p, u],
                        -nodeValue[c, k, ch, u]
                    )

    comment("None-type nodes have False values")
    // (nodeType[p] = NONE) => AND_{u}( ~nodeValue[p,u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                imply(
                    nodeType[c, k, p, NodeType.NONE.value],
                    -nodeValue[c, k, p, u]
                )
}
