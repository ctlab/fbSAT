package ru.ifmo.fbsat.core.task.single.extprecomp

import com.github.lipen.satlib.card.Cardinality
import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.DomainVarArray
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.eq
import com.github.lipen.satlib.core.neq
import com.github.lipen.satlib.core.sign
import com.github.lipen.satlib.op.iff
import com.github.lipen.satlib.op.imply
import com.github.lipen.satlib.op.implyAnd
import com.github.lipen.satlib.op.implyIff
import com.github.lipen.satlib.op.implyIffAnd
import com.github.lipen.satlib.op.implyIffOr
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.pairs
import ru.ifmo.fbsat.core.utils.triples

// TODO: consider renaming to 'extended-nnf'
data class ExtPreCompTask(
    val maxTotalGuardsSize: Int? = null, // N, unconstrained if null
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        comment("$name: Variables")
        declareExtPreCompVariables()

        /* Constraints */
        comment("$name: Constraints")
        declareExtPreCompGuardConditionsConstraints()

        /* Initial cardinality constraints */
        comment("$name: Initial cardinality (N) constraints")
        val cardinalityN: Cardinality = context["cardinalityN"]
        if (Globals.IS_USE_ASSUMPTIONS) {
            cardinalityN.assumeUpperBoundLessThanOrEqual(maxTotalGuardsSize)
        } else {
            cardinalityN.declareUpperBoundLessThanOrEqual(maxTotalGuardsSize)
        }
    }
}

fun Solver.declareExtPreCompGuardConditionsConstraints() {
    comment("ExtPreComp guard conditions constraints")
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val uniqueInputs = scenarioTree.uniqueInputs
    val C: Int = context["C"]
    val K: Int = context["K"]
    val X: Int = context["X"]
    val U: Int = context["U"]
    val transitionDestination: IntVarArray = context["transitionDestination"]
    val transitionTruthTable: BoolVarArray = context["transitionTruthTable"]
    val guardType: DomainVarArray<GuardType> = context["guardType"]
    val guardTerminalInputVariable: IntVarArray = context["guardTerminalInputVariable"]
    val guardTerminalValue: BoolVarArray = context["guardTerminalValue"]
    val guardTerminalNegation: BoolVarArray = context["guardTerminalNegation"]
    val guardSize: IntVarArray = context["guardSize"]

    comment("Only null-transitions have no guard")
    for (c in 1..C)
        for (k in 1..K)
            iff(
                transitionDestination[c, k] eq 0,
                guardType[c, k] eq GuardType.NoGuard
            )

    comment("No-var terminal propagation")
    for (c in 1..C)
        for (k in 1..K)
            for (i in 1 until 3)
                imply(
                    guardTerminalInputVariable[c, k, i] eq 0,
                    guardTerminalInputVariable[c, k, i + 1] eq 0,
                )

    comment("No-var terminals are not negated [deterministic constraint]")
    for (c in 1..C)
        for (k in 1..K)
            for (i in 1..3)
                imply(
                    guardTerminalInputVariable[c, k, i] eq 0,
                    -guardTerminalNegation[c, k, i],
                )

    comment("No-var terminal value is false [deterministic constraint]")
    for (c in 1..C)
        for (k in 1..K)
            for (i in 1..3)
                for (u in 1..U)
                    imply(
                        guardTerminalInputVariable[c, k, i] eq 0,
                        -guardTerminalValue[c, k, i, u]
                    )

    comment("Terminal value")
    // (guardVar[c,k]=x) & guardTerminalNegation[c,k,i] => (guardTerminalValue[c,k,i,u] == ~u[x])
    // (guardVar[c,k]=x) & ~guardTerminalNegation[c,k,i] => (guardTerminalValue[c,k,i,u] == u[x])
    for (c in 1..C)
        for (k in 1..K)
            for (x in 1..X)
                for (i in 1..3)
                    for (u in 1..U) {
                        clause(
                            -(guardTerminalInputVariable[c, k, i] eq x),
                            -(guardTerminalNegation[c, k, i]),
                            guardTerminalValue[c, k, i, u] sign !uniqueInputs[u - 1][x - 1]
                        )
                        clause(
                            -(guardTerminalInputVariable[c, k, i] eq x),
                            -(-guardTerminalNegation[c, k, i]),
                            guardTerminalValue[c, k, i, u] sign uniqueInputs[u - 1][x - 1]
                        )
                    }

    comment("GuardType::arity encoding")
    for (c in 1..C)
        for (k in 1..K)
            for (t in GuardType.values()) {
                for (i in 1..t.arity) {
                    imply(
                        guardType[c, k] eq t,
                        guardTerminalInputVariable[c, k, i] neq 0,
                    )
                }
                if (t.arity + 1 <= guardTerminalInputVariable.shape[2]) {
                    imply(
                        guardType[c, k] eq t,
                        guardTerminalInputVariable[c, k, t.arity + 1] eq 0
                    )
                }
            }

    comment("GuardType.NoGuard encoding")
    for (c in 1..C)
        for (k in 1..K)
            for (u in 1..U)
                imply(
                    guardType[c, k] eq GuardType.NoGuard,
                    -transitionTruthTable[c, k, u]
                )

    comment("GuardType.True encoding")
    for (c in 1..C)
        for (k in 1..K)
            for (u in 1..U)
                imply(
                    guardType[c, k] eq GuardType.True,
                    transitionTruthTable[c, k, u]
                )

    comment("GuardType.Var encoding")
    // (guardType[c,k]=Var) => (truthTable[c,k,u] == guardTerminalValue[c,k,1,u])
    for (c in 1..C)
        for (k in 1..K)
            for (u in 1..U)
                implyIff(
                    guardType[c, k] eq GuardType.Var,
                    transitionTruthTable[c, k, u],
                    guardTerminalValue[c, k, 1, u]
                )

    comment("GuardType.And2 encoding")
    // (guardType[c,k]=And2) => (truthTable[c,k,u] == guardTerminalValue[c,k,1,u] /\ ...[2])
    for (c in 1..C)
        for (k in 1..K)
            for (u in 1..U)
                implyIffAnd(
                    guardType[c, k] eq GuardType.And2,
                    transitionTruthTable[c, k, u],
                    guardTerminalValue[c, k, 1, u],
                    guardTerminalValue[c, k, 2, u]
                )

    comment("GuardType.Or2 encoding")
    // (guardType[c,k]=Or2) => (truthTable[c,k,u] == guardTerminalValue[c,k,1,u] \/ ...[2])
    for (c in 1..C)
        for (k in 1..K)
            for (u in 1..U)
                implyIffOr(
                    guardType[c, k] eq GuardType.Or2,
                    transitionTruthTable[c, k, u],
                    guardTerminalValue[c, k, 1, u],
                    guardTerminalValue[c, k, 2, u]
                )

    comment("GuardType.And3 encoding")
    // (guardType[c,k]=And3) => (truthTable[c,k,u] == guardTerminalValue[c,k,1,u] /\ ...[2] /\ ...[3])
    for (c in 1..C)
        for (k in 1..K)
            for (u in 1..U)
                implyIffAnd(
                    guardType[c, k] eq GuardType.And3,
                    transitionTruthTable[c, k, u],
                    guardTerminalValue[c, k, 1, u],
                    guardTerminalValue[c, k, 2, u],
                    guardTerminalValue[c, k, 3, u]
                )

    comment("ADHOC: And3 cannot have negated terminals")
    for (c in 1..C)
        for (k in 1..K)
            implyAnd(
                guardType[c, k] eq GuardType.And3,
                -guardTerminalNegation[c, k, 1],
                -guardTerminalNegation[c, k, 2],
                -guardTerminalNegation[c, k, 3],
            )

    comment("Formula size encoding")
    for (c in 1..C)
        for (k in 1..K) {
            // NoGuard
            imply(
                guardType[c, k] eq GuardType.NoGuard,
                guardSize[c, k] eq 0
            )

            // True
            imply(
                guardType[c, k] eq GuardType.True,
                guardSize[c, k] eq 1
            )

            // Var (positive)
            clause(
                -(guardType[c, k] eq GuardType.Var),
                -(-guardTerminalNegation[c, k, 1]),
                guardSize[c, k] eq 1
            )
            // Var (negative)
            clause(
                -(guardType[c, k] eq GuardType.Var),
                -(guardTerminalNegation[c, k, 1]),
                guardSize[c, k] eq 2
            )

            // And2
            clause(
                -(guardType[c, k] eq GuardType.And2),
                -(-guardTerminalNegation[c, k, 1]),
                -(-guardTerminalNegation[c, k, 2]),
                guardSize[c, k] eq 3
            )
            clause(
                -(guardType[c, k] eq GuardType.And2),
                -(-guardTerminalNegation[c, k, 1]),
                -(guardTerminalNegation[c, k, 2]),
                guardSize[c, k] eq 4
            )
            clause(
                -(guardType[c, k] eq GuardType.And2),
                -(guardTerminalNegation[c, k, 1]),
                -(-guardTerminalNegation[c, k, 2]),
                guardSize[c, k] eq 4
            )
            clause(
                -(guardType[c, k] eq GuardType.And2),
                -(guardTerminalNegation[c, k, 1]),
                -(guardTerminalNegation[c, k, 2]),
                guardSize[c, k] eq 5
            )

            // Or2
            clause(
                -(guardType[c, k] eq GuardType.Or2),
                -(-guardTerminalNegation[c, k, 1]),
                -(-guardTerminalNegation[c, k, 2]),
                guardSize[c, k] eq 3
            )
            clause(
                -(guardType[c, k] eq GuardType.Or2),
                -(-guardTerminalNegation[c, k, 1]),
                -(guardTerminalNegation[c, k, 2]),
                guardSize[c, k] eq 4
            )
            clause(
                -(guardType[c, k] eq GuardType.Or2),
                -(guardTerminalNegation[c, k, 1]),
                -(-guardTerminalNegation[c, k, 2]),
                guardSize[c, k] eq 4
            )
            clause(
                -(guardType[c, k] eq GuardType.Or2),
                -(guardTerminalNegation[c, k, 1]),
                -(guardTerminalNegation[c, k, 2]),
                guardSize[c, k] eq 5
            )

            // And3
            implyAnd(
                guardType[c, k] eq GuardType.And3,
                guardSize[c, k] eq 5
            )
        }
}

fun Solver.declareExtPreCompGuardConditionsConstraints_OLD() {
    comment("ExtPreComp guard conditions constraints")
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val uniqueInputs = scenarioTree.uniqueInputs
    val C: Int = context["C"]
    val K: Int = context["K"]
    val X: Int = context["X"]
    val U: Int = context["U"]
    val transitionDestination: IntVarArray = context["transitionDestination"]
    val transitionTruthTable: BoolVarArray = context["transitionTruthTable"]
    val transitionGuardFormula: IntVarArray = context["transitionGuardFormula"]
    val transitionGuardFormulaNode: BoolVarArray = context["transitionGuardFormulaNode"]

    comment("Only null-transitions have no guard (formula is 0)")
    for (c in 1..C) {
        for (k in 1..K) {
            iff(
                transitionDestination[c, k] eq 0,
                transitionGuardFormula[c, k] eq 0
            )
        }
    }

    comment("Formulae encodings")
    for (c in 1..C) {
        for (k in 1..K) {
            for (u in 1..U) {
                // 1 is unconditional guard
                var f = 1
                imply(
                    transitionGuardFormula[c, k] eq f,
                    transitionTruthTable[c, k, u]
                )
                check(f == 1)

                // next X formulae: positive variable
                for (x in 1..X) {
                    f++
                    imply(
                        transitionGuardFormula[c, k] eq f,
                        transitionTruthTable[c, k, u] sign uniqueInputs[u - 1][x - 1]
                    )
                }
                check(f == 1 + X)

                // next X formulae: negative variable
                for (x in 1..X) {
                    f++
                    imply(
                        transitionGuardFormula[c, k] eq f,
                        transitionTruthTable[c, k, u] sign !uniqueInputs[u - 1][x - 1]
                    )
                }
                check(f == 1 + 2 * X)

                // next (X*(X+1)/2) formulae: conjunction of 2 positive variables
                for ((x1, x2) in (1..X).pairs()) {
                    f++
                    val a = uniqueInputs[u - 1][x1 - 1]
                    val b = uniqueInputs[u - 1][x2 - 1]
                    imply(
                        transitionGuardFormula[c, k] eq f,
                        transitionTruthTable[c, k, u] sign (a && b)
                    )
                }
                check(f == 1 + 2 * X + (X * (X - 1) / 2))

                // next (X*(X+1)/2) formulae: conjunction of 2 negative variables
                for ((x1, x2) in (1..X).pairs()) {
                    f++
                    val a = uniqueInputs[u - 1][x1 - 1]
                    val b = uniqueInputs[u - 1][x2 - 1]
                    imply(
                        transitionGuardFormula[c, k] eq f,
                        transitionTruthTable[c, k, u] sign (!a && !b)
                    )
                }
                check(f == 1 + 2 * X + 2 * (X * (X - 1) / 2))

                // next (X*(X+1)/2) formulae: conjunction of 1 positive and 1 negative variable
                for ((x1, x2) in (1..X).pairs()) {
                    f++
                    val a = uniqueInputs[u - 1][x1 - 1]
                    val b = uniqueInputs[u - 1][x2 - 1]
                    imply(
                        transitionGuardFormula[c, k] eq f,
                        transitionTruthTable[c, k, u] sign (a && !b)
                    )
                }
                check(f == 1 + 2 * X + 3 * (X * (X - 1) / 2))

                // next (X*(X+1)/2) formulae: conjunction of 1 negative and 1 positive variable
                for ((x1, x2) in (1..X).pairs()) {
                    f++
                    val a = uniqueInputs[u - 1][x1 - 1]
                    val b = uniqueInputs[u - 1][x2 - 1]
                    imply(
                        transitionGuardFormula[c, k] eq f,
                        transitionTruthTable[c, k, u] sign (!a && b)
                    )
                }
                check(f == 1 + 2 * X + 4 * (X * (X - 1) / 2))

                // next (X*(X+1)/2) formulae: disjunction of 2 positive variables
                for ((x1, x2) in (1..X).pairs()) {
                    f++
                    val a = uniqueInputs[u - 1][x1 - 1]
                    val b = uniqueInputs[u - 1][x2 - 1]
                    imply(
                        transitionGuardFormula[c, k] eq f,
                        transitionTruthTable[c, k, u] sign (a || b)
                    )
                }
                check(f == 1 + 2 * X + 5 * (X * (X - 1) / 2))

                // next (X*(X-1)*(X-2)/6) formulae: conjunction of 3 positive variables
                for ((x1, x2, x3) in (1..X).triples()) {
                    f++
                    val v1 = uniqueInputs[u - 1][x1 - 1]
                    val v2 = uniqueInputs[u - 1][x2 - 1]
                    val v3 = uniqueInputs[u - 1][x3 - 1]
                    imply(
                        transitionGuardFormula[c, k] eq f,
                        transitionTruthTable[c, k, u] sign (v1 && v2 && v3)
                    )
                }
                check(f == 1 + 2 * X + 5 * (X * (X - 1) / 2) + (X * (X - 1) * (X - 2) / 6))
            }
        }
    }

    comment("Formula size encoding (used nodes)")
    for (c in 1..C) {
        for (k in 1..K) {
            // 0
            implyAnd(
                transitionGuardFormula[c, k] eq 0,
                -transitionGuardFormulaNode[c, k, 1],
                -transitionGuardFormulaNode[c, k, 2],
                -transitionGuardFormulaNode[c, k, 3],
                -transitionGuardFormulaNode[c, k, 4],
                -transitionGuardFormulaNode[c, k, 5],
            )

            // 1
            implyAnd(
                transitionGuardFormula[c, k] eq 1,
                transitionGuardFormulaNode[c, k, 1],
                -transitionGuardFormulaNode[c, k, 2],
                -transitionGuardFormulaNode[c, k, 3],
                -transitionGuardFormulaNode[c, k, 4],
                -transitionGuardFormulaNode[c, k, 5],
            )

            var f = 1

            // next X formulae: positive variable
            for (x in 1..X) {
                f++
                implyAnd(
                    transitionGuardFormula[c, k] eq f,
                    transitionGuardFormulaNode[c, k, 1],
                    -transitionGuardFormulaNode[c, k, 2],
                    -transitionGuardFormulaNode[c, k, 3],
                    -transitionGuardFormulaNode[c, k, 4],
                    -transitionGuardFormulaNode[c, k, 5],
                )
            }
            check(f == 1 + X)

            // next X formulae: negative variable
            for (x in 1..X) {
                f++
                implyAnd(
                    transitionGuardFormula[c, k] eq f,
                    transitionGuardFormulaNode[c, k, 1],
                    transitionGuardFormulaNode[c, k, 2],
                    -transitionGuardFormulaNode[c, k, 3],
                    -transitionGuardFormulaNode[c, k, 4],
                    -transitionGuardFormulaNode[c, k, 5],
                )
            }
            check(f == 1 + 2 * X)

            // next (X*(X-1)/2) formulae: conjunction of 2 positive variables
            for ((x1, x2) in (1..X).pairs()) {
                f++
                implyAnd(
                    transitionGuardFormula[c, k] eq f,
                    transitionGuardFormulaNode[c, k, 1],
                    transitionGuardFormulaNode[c, k, 2],
                    transitionGuardFormulaNode[c, k, 3],
                    -transitionGuardFormulaNode[c, k, 4],
                    -transitionGuardFormulaNode[c, k, 5],
                )
            }
            check(f == 1 + 2 * X + (X * (X - 1) / 2))

            // next (X*(X-1)/2) formulae: conjunction of 2 negative variables
            for ((x1, x2) in (1..X).pairs()) {
                f++
                implyAnd(
                    transitionGuardFormula[c, k] eq f,
                    transitionGuardFormulaNode[c, k, 1],
                    transitionGuardFormulaNode[c, k, 2],
                    transitionGuardFormulaNode[c, k, 3],
                    transitionGuardFormulaNode[c, k, 4],
                    transitionGuardFormulaNode[c, k, 5],
                )
            }
            check(f == 1 + 2 * X + 2 * (X * (X - 1) / 2))

            // next (X*(X-1)/2) formulae: conjunction of 1 positive and 1 negative variable
            for ((x1, x2) in (1..X).pairs()) {
                f++
                implyAnd(
                    transitionGuardFormula[c, k] eq f,
                    transitionGuardFormulaNode[c, k, 1],
                    transitionGuardFormulaNode[c, k, 2],
                    transitionGuardFormulaNode[c, k, 3],
                    transitionGuardFormulaNode[c, k, 4],
                    -transitionGuardFormulaNode[c, k, 5],
                )
            }
            check(f == 1 + 2 * X + 3 * (X * (X - 1) / 2))

            // next (X*(X-1)/2) formulae: conjunction of 1 negative and 1 positive variable
            for ((x1, x2) in (1..X).pairs()) {
                f++
                implyAnd(
                    transitionGuardFormula[c, k] eq f,
                    transitionGuardFormulaNode[c, k, 1],
                    transitionGuardFormulaNode[c, k, 2],
                    transitionGuardFormulaNode[c, k, 3],
                    transitionGuardFormulaNode[c, k, 4],
                    -transitionGuardFormulaNode[c, k, 5],
                )
            }
            check(f == 1 + 2 * X + 4 * (X * (X - 1) / 2))

            // next (X*(X-1)/2) formulae: disjunction of 2 positive variables
            for ((x1, x2) in (1..X).pairs()) {
                f++
                implyAnd(
                    transitionGuardFormula[c, k] eq f,
                    transitionGuardFormulaNode[c, k, 1],
                    transitionGuardFormulaNode[c, k, 2],
                    transitionGuardFormulaNode[c, k, 3],
                    -transitionGuardFormulaNode[c, k, 4],
                    -transitionGuardFormulaNode[c, k, 5],
                )
            }
            check(f == 1 + 2 * X + 5 * (X * (X - 1) / 2))

            // next (X*(X-1)*(X-2)/6) formulae: conjunction of 3 positive variables
            for ((x1, x2, x3) in (1..X).triples()) {
                f++
                implyAnd(
                    transitionGuardFormula[c, k] eq f,
                    transitionGuardFormulaNode[c, k, 1],
                    transitionGuardFormulaNode[c, k, 2],
                    transitionGuardFormulaNode[c, k, 3],
                    transitionGuardFormulaNode[c, k, 4],
                    transitionGuardFormulaNode[c, k, 5],
                )
            }
            check(f == 1 + 2 * X + 5 * (X * (X - 1) / 2) + (X * (X - 1) * (X - 2) / 6))
        }
    }
}
