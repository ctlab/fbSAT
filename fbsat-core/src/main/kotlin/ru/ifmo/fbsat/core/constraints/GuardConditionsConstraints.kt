package ru.ifmo.fbsat.core.constraints

import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.DomainVarArray
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.SolverContext
import ru.ifmo.fbsat.core.solver.iff
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.implyAnd
import ru.ifmo.fbsat.core.solver.implyImply
import ru.ifmo.fbsat.core.solver.implyImplyIff
import ru.ifmo.fbsat.core.solver.implyImplyIffAnd
import ru.ifmo.fbsat.core.solver.implyImplyIffOr
import ru.ifmo.fbsat.core.solver.sign
import ru.ifmo.fbsat.core.solver.switchContext
import ru.ifmo.fbsat.core.task.single.extforest.ck2p
import ru.ifmo.fbsat.core.utils.Globals

fun Solver.declarePositiveGuardConditionsConstraints() {
    comment("Positive guard conditions constraints")
    val U: Int by context

    comment("Positive guard conditions constraints: inputless")
    declareGuardConditionsConstraintsInputless()

    comment("Positive guard conditions constraints: for inputs (${1..U})")
    val positiveScenarioTree: PositiveScenarioTree by context
    context["tree"] = positiveScenarioTree
    declareGuardConditionsConstraintsForInputs(Us = 1..U)
}

fun Solver.declareNegativeGuardConditionsConstraints(Us: Iterable<Int>) {
    comment("Negative guard conditions constraints")
    val negativeContext: SolverContext by context

    // Note: no inputless constraints

    comment("Negative guard conditions constraints: for inputs ($Us)")
    val negativeScenarioTree: NegativeScenarioTree by context
    switchContext(negativeContext) {
        context["tree"] = negativeScenarioTree
        declareGuardConditionsConstraintsForInputs(Us = Us)
    }
}

fun Solver.declareParallelModularGuardConditionsConstraints() {
    // comment("Parallel modular guard conditions constraints")
    // with(parallelModularExtendedVariables) {
    //     for (m in 1..M) {
    //         comment("Parallel modular guard conditions constraints: for module m = $m")
    //         declarePositiveGuardConditionsConstraints(
    //             extendedVars = modularExtendedVariables[m]
    //         )
    //     }
    // }
}

fun Solver.declareConsecutiveModularGuardConditionsConstraints() {
    // comment("Consecutive modular guard conditions constraints")
    // with(consecutiveModularExtendedVariables) {
    //     for (m in 1..M) {
    //         comment("Consecutive modular guard conditions constraints: for module m = $m")
    //         declarePositiveGuardConditionsConstraints(
    //             extendedVars = modularExtendedVariables[m]
    //         )
    //     }
    // }
}

fun Solver.declareDistributedPositiveGuardConditionsConstraints() {
    // comment("Distributed guard conditions constraints")
    // with(distributedExtendedVariables) {
    //     for (m in 1..M) {
    //         comment("Distributed guard conditions constraints: for module m = $m")
    //         declarePositiveGuardConditionsConstraints(
    //             extendedVars = modularExtendedVariables[m]
    //         )
    //
    //         // FIXME: this should be called from 'declareDistributedGuardConditionsAdhocConstraints' method
    //         comment("Distributed guard conditions adhoc constraints: for module m = $m")
    //         declareGuardConditionsAdhocConstraints(
    //             extendedVars = modularExtendedVariables[m]
    //         )
    //     }
    // }
}

fun Solver.declareGuardConditionsAdhocConstraints() {
    comment("Adhoc guard conditions constraints")
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val X: Int by context
    val nodeType: DomainVarArray<NodeType> by context
    val nodeInputVariable: IntVarArray by context
    val nodeChild: IntVarArray by context

    comment("Forbid double negation")
    // (nodeType[p] = NOT) & (nodeChild[p] = ch) => (nodeType[ch] != NOT)
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1 until P)
                for (ch in (p + 1)..P)
                    implyImply(
                        nodeType[c, k, p] eq NodeType.NOT,
                        nodeChild[c, k, p] eq ch,
                        nodeType[c, k, ch] neq NodeType.NOT
                    )

    comment("Distinct transitions")
    // TODO: Distinct transitions

    if (Globals.IS_FORBID_OR) {
        comment("Forbid ORs")
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..P)
                    clause(nodeType[c, k, p] neq NodeType.OR)
    }

    if (Globals.IS_ENCODE_TERMINALS_ORDER) {
        comment("Terminals order")
        // terminal[p, x] => AND_{p'<p, x'>=x}( ~terminal[r_, x_] )
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..P)
                    for (x in 1..X)
                        implyAnd(nodeInputVariable[c, k, p] eq x, sequence {
                            for (p_ in 1 until p)
                                for (x_ in x..X)
                                    yield(nodeInputVariable[c, k, p_] neq x_)
                        })
    }

    if (Globals.IS_ENCODE_TERMINALS_MINI_ORDER) {
        // Note: this constraint seems to be very expensive, but does not provide visible speed-up
        comment("Terminals mini-order: AND/OR children-terminals order")
        // (nodeType[p] = AND/OR) & (nodeChild[p] = ch) & (nodeType[ch] = TERMINAL) & (nodeType[ch+1] = TERMINAL) => (nodeInputVariable[ch] < nodeInputVariable[ch+1])
        for (c in 1..C)
            for (k in 1..K)
                for (p in 1..P)
                    for (t in listOf(NodeType.AND, NodeType.OR))
                        for (ch in (p + 1) until P)
                            for (x in 1..X)
                                for (x_ in 1..x)
                                    clause(
                                        nodeType[c, k, p] neq t,
                                        nodeChild[c, k, p] neq ch,
                                        nodeType[c, k, ch] neq NodeType.NOT,
                                        nodeType[c, k, ch + 1] neq NodeType.NOT,
                                        nodeInputVariable[c, k, ch] neq x,
                                        nodeInputVariable[c, k, ch + 1] neq x_
                                    )
    }
}

private fun Solver.declareGuardConditionsConstraintsInputless() {
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val transitionDestination: IntVarArray by context
    val nodeType: DomainVarArray<NodeType> by context
    val nodeInputVariable: IntVarArray by context
    val nodeParent: IntVarArray by context
    val nodeChild: IntVarArray by context

    comment("None-typed nodes have largest indices")
    // (nodeType[p] = NONE) => (nodeType[p+1] = NONE)
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1 until P)
                imply(
                    nodeType[c, k, p] eq NodeType.NONE,
                    nodeType[c, k, p + 1] eq NodeType.NONE
                )

    comment("Only null-transitions have no guard (root is none-typed)")
    // (transitionDestination = 0) <=> (nodeType[1] = NONE)
    for (c in 1..C)
        for (k in 1..K)
            iff(
                transitionDestination[c, k] eq 0,
                nodeType[c, k, 1] eq NodeType.NONE
            )

    comment("child=>parent relation")
    // (nodeChild[p] = ch) => (nodeParent[ch] = p)
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (ch in (p + 1)..P)
                    imply(
                        nodeChild[c, k, p] eq ch,
                        nodeParent[c, k, ch] eq p
                    )
    // (nodeChild[p] = 0) => AND_{ch}(nodeParent[ch] != p)
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                implyAnd(nodeChild[c, k, p] eq 0, sequence {
                    for (ch in (p + 1)..P)
                        yield(nodeParent[c, k, ch] neq p)
                })

    comment("Only typed nodes, except the root, have parents")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 2..P)
                iff(
                    nodeParent[c, k, p] neq 0,
                    nodeType[c, k, p] neq NodeType.NONE
                )

    // TERMINALS CONSTRAINTS

    comment("Only terminal nodes have associated input variables")
    // (nodeType[p] = TERMINAL) <=> (nodeInputVariable[p] != 0)
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                iff(
                    nodeType[c, k, p] eq NodeType.TERMINAL,
                    nodeInputVariable[c, k, p] neq 0
                )

    comment("Terminals do not have children")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                imply(
                    nodeType[c, k, p] eq NodeType.TERMINAL,
                    nodeChild[c, k, p] eq 0
                )

    // AND/OR NODES CONSTRAINTS

    comment("11.0a. AND/OR nodes cannot have numbers P-1 or P")
    for (c in 1..C)
        for (k in 1..K) {
            if (P >= 1) {
                clause(nodeType[c, k, P] neq NodeType.AND)
                clause(nodeType[c, k, P] neq NodeType.OR)
            }
            if (P >= 2) {
                clause(nodeType[c, k, P - 1] neq NodeType.AND)
                clause(nodeType[c, k, P - 1] neq NodeType.OR)
            }
        }

    comment("11.0b. AND/OR: left child cannot have number P")
    // (nodeType[p] = AND/OR) => (nodeChild[p] != P)
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (nt in listOf(NodeType.AND, NodeType.OR))
                    imply(
                        nodeType[c, k, p] eq nt,
                        nodeChild[c, k, p] neq P
                    )

    comment("11.1. AND/OR nodes have left child")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..(P - 2))
                for (nt in listOf(NodeType.AND, NodeType.OR))
                    imply(
                        nodeType[c, k, p] eq nt,
                        nodeChild[c, k, p] neq 0
                    )

    if (Globals.IS_ENCODE_HARD_TO_EXPLAIN) {
        comment("11.+. AND/OR: hard to explain")
        // parent[p, par] & nodetype[par, AND/OR] => child[par, p] | child[par, p-1]
        for (c in 1..C)
            for (k in 1..K)
                for (par in 1..P) {
                    run {
                        val ch = par + 1
                        if (ch < P)
                            for (nt in listOf(NodeType.AND, NodeType.OR))
                                clause(
                                    nodeType[c, k, par] neq nt,
                                    nodeParent[c, k, ch] neq par,
                                    nodeChild[c, k, par] eq ch
                                )
                    }
                    for (ch in (par + 2) until P)
                        for (nt in listOf(NodeType.AND, NodeType.OR))
                            clause(
                                nodeType[c, k, par] neq nt,
                                nodeParent[c, k, ch] neq par,
                                nodeChild[c, k, par] eq ch,
                                nodeChild[c, k, par] eq ch - 1
                            )
                }
    }

    comment("Right child of binary operators follows the left one")
    // (nodeType[p] = AND/OR) & (nodeChild[p] = ch) => (nodeParent[ch+1] = p)
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (ch in (p + 1) until P)
                    for (nt in listOf(NodeType.AND, NodeType.OR))
                        implyImply(
                            nodeType[c, k, p] eq nt,
                            nodeChild[c, k, p] eq ch,
                            nodeParent[c, k, ch + 1] eq p
                        )

    // NOT NODES CONSTRAINTS

    comment("12.0. NOT nodes cannot have number P")
    for (c in 1..C)
        for (k in 1..K)
            if (P >= 1)
                clause(nodeType[c, k, P] neq NodeType.NOT)

    comment("12.1. NOT nodes have left child")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1 until P)
                imply(
                    nodeType[c, k, p] eq NodeType.NOT,
                    nodeChild[c, k, p] neq 0
                )

    comment("12.2. NOT: parent's child is the current node")
    // parent[p, par] & nodetype[par, NOT] => child[par, p]
    for (c in 1..C)
        for (k in 1..K)
            for (par in 1..P)
                for (ch in (par + 1)..P)
                    implyImply(
                        nodeType[c, k, par] eq NodeType.NOT,
                        nodeParent[c, k, ch] eq par,
                        nodeChild[c, k, par] eq ch
                    )

    // NONE-TYPE NODES CONSTRAINTS

    comment("None-typed nodes do not have children")
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                imply(
                    nodeType[c, k, p] eq NodeType.NONE,
                    nodeChild[c, k, p] eq 0
                )
}

private fun Solver.declareGuardConditionsConstraintsForInputs(Us: Iterable<Int>) {
    val tree: ScenarioTree<*, *> by context
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val X: Int by context
    val nodeType: DomainVarArray<NodeType> by context
    val nodeInputVariable: IntVarArray by context
    val nodeChild: IntVarArray by context
    val nodeValue: BoolVarArray by context

    comment("Terminal nodes have value from associated input variables")
    // (nodeInputVariable[p] = x) => AND_{u}( nodeValue[p,u] <=> u[x] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (x in 1..X)
                    for (u in Us)
                        imply(
                            nodeInputVariable[c, k, p] eq x,
                            nodeValue[c, k, p, u] sign tree.uniqueInputs[u - 1][x - 1]
                        )

    comment("AND: value is calculated as a conjunction of children values")
    // (nodeType[p] = AND) & (nodeChild[p] = ch) =>
    //  AND_{u}( nodeValue[p,u] <=> nodeValue[ch,u] & nodeValue[ch+1,u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (ch in (p + 1) until P)
                    for (u in Us)
                        implyImplyIffAnd(
                            nodeType[c, k, p] eq NodeType.AND,
                            nodeChild[c, k, p] eq ch,
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
                    for (u in Us)
                        implyImplyIffOr(
                            nodeType[c, k, p] eq NodeType.OR,
                            nodeChild[c, k, p] eq ch,
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
                    for (u in Us)
                        implyImplyIff(
                            nodeType[c, k, p] eq NodeType.NOT,
                            nodeChild[c, k, p] eq ch,
                            nodeValue[c, k, p, u],
                            -nodeValue[c, k, ch, u]
                        )

    comment("None-type nodes have False values")
    // (nodeType[p] = NONE) => AND_{u}( ~nodeValue[p,u] )
    for (c in 1..C)
        for (k in 1..K)
            for (p in 1..P)
                for (u in Us)
                    imply(
                        nodeType[c, k, p] eq NodeType.NONE,
                        -nodeValue[c, k, p, u]
                    )
}

fun Solver.declareExtForestGuardConditionsConstraints() {
    comment("ExtForest guard conditions constraints")
    val scenarioTree: PositiveScenarioTree by context
    val C: Int by context
    val K: Int by context
    val P: Int by context
    val X: Int by context
    val U: Int by context
    val transitionDestination: IntVarArray by context
    val nodeType: DomainVarArray<NodeType> by context
    val nodeInputVariable: IntVarArray by context
    val nodeParent: IntVarArray by context
    val nodeChild: IntVarArray by context
    val nodeValue: BoolVarArray by context

    comment("Parent-child relation")
    // (nodeChild[p] = ch) => (nodeParent[ch] = p)
    for (p in 1..P)
        for (ch in nodeChild[p].domain - 0)
            imply(
                nodeChild[p] eq ch,
                nodeParent[ch] eq p
            )

    comment("Only null-transitions have no guard (root is none-typed)")
    // (transitionDestination[c,k] = 0) <=> (nodeType[p] = NONE)
    for (c in 1..C)
        for (k in 1..K) {
            val p = ck2p(c, k, K)
            iff(
                transitionDestination[c, k] eq 0,
                nodeType[p] eq NodeType.NONE
            )
        }

    // Note: these constrains are redundant, but do not make anything bad (minor speedup sometimes)
    comment("Types constraints for last nodes")
    clause(nodeType[P] neq NodeType.AND)
    clause(nodeType[P] neq NodeType.OR)
    clause(nodeType[P] neq NodeType.NOT)
    clause(nodeType[P - 1] neq NodeType.AND)
    clause(nodeType[P - 1] neq NodeType.OR)

    comment("TERMINAL: no children")
    for (p in 1..P)
        imply(
            nodeType[p] eq NodeType.TERMINAL,
            nodeChild[p] eq 0
        )

    comment("TERMINAL: input variable")
    for (p in 1..P)
        iff(
            nodeType[p] eq NodeType.TERMINAL,
            nodeInputVariable[p] neq 0
        )

    comment("TERMINAL: value")
    for (p in 1..P)
        for (x in 1..X)
            for (u in 1..U)
                imply(
                    nodeInputVariable[p] eq x,
                    nodeValue[p, u] sign scenarioTree.uniqueInputs[u - 1][x - 1]
                )

    comment("AND/OR/NOT: left child")
    for (p in 1..P)
        for (t in listOf(NodeType.AND, NodeType.OR, NodeType.NOT))
            imply(
                nodeType[p] eq t,
                nodeChild[p] neq 0
            )
    for (p in 1 until P)
        for (t in listOf(NodeType.AND, NodeType.OR))
            imply(
                nodeType[p] eq t,
                nodeChild[p] neq P
            )

    comment("AND/OR: right child")
    for (p in 1..P)
        for (ch in nodeChild[p].domain - 0)
            if (ch <= P - 1)
                for (t in listOf(NodeType.AND, NodeType.OR))
                    implyImply(
                        nodeType[p] eq t,
                        nodeChild[p] eq ch,
                        nodeParent[ch + 1] eq p
                    )

    comment("AND: value")
    for (p in 1..P)
        for (ch in nodeChild[p].domain - 0)
            if (ch <= P - 1)
                for (u in 1..U)
                    implyImplyIffAnd(
                        nodeType[p] eq NodeType.AND,
                        nodeChild[p] eq ch,
                        nodeValue[p, u],
                        nodeValue[ch, u],
                        nodeValue[ch + 1, u]
                    )

    comment("OR: value")
    for (p in 1..P)
        for (ch in nodeChild[p].domain - 0)
            if (ch <= P - 1)
                for (u in 1..U)
                    implyImplyIffOr(
                        nodeType[p] eq NodeType.OR,
                        nodeChild[p] eq ch,
                        nodeValue[p, u],
                        nodeValue[ch, u],
                        nodeValue[ch + 1, u]
                    )

    comment("NOT: value")
    for (p in 1..P)
        for (ch in nodeChild[p].domain - 0)
            for (u in 1..U)
                implyImplyIff(
                    nodeType[p] eq NodeType.NOT,
                    nodeChild[p] eq ch,
                    nodeValue[p, u],
                    -nodeValue[ch, u]
                )

    // Note: this constraint does a major slowdown
    // comment("NOT: forbid double-negation")
    // for (p in 1..P)
    //     for (ch in nodeChild[p].domain - 0 - P)
    //         implyImply(
    //             nodeType[p] eq NodeType.NOT,
    //             nodeChild[p] eq ch,
    //             nodeType[ch] neq NodeType.NOT
    //         )

    comment("NONE: propagation")
    for (p in (C * K + 1) until P)
        imply(
            nodeType[p] eq NodeType.NONE,
            nodeType[p + 1] eq NodeType.NONE
        )

    comment("NONE: no parent -- only for (C*K+1)..P")
    for (p in (C * K + 1)..P)
        iff(
            nodeType[p] eq NodeType.NONE,
            nodeParent[p] eq 0
        )

    comment("NONE: no children")
    for (p in 1..P)
        imply(
            nodeType[p] eq NodeType.NONE,
            nodeChild[p] eq 0
        )

    comment("NONE: value is false")
    for (p in 1..P)
        for (u in 1..U)
            imply(
                nodeType[p] eq NodeType.NONE,
                -nodeValue[p, u]
            )

    comment("BFS")
    for (p in 1..P)
        for (ch in nodeChild[p].domain - 0)
            if (ch < P)
                implyAnd(nodeParent[ch] eq p, sequence {
                    for (s in 1 until p)
                        yield(nodeParent[ch + 1] neq s)
                })
}
