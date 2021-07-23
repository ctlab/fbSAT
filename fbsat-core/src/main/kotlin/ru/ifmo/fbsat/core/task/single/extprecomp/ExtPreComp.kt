package ru.ifmo.fbsat.core.task.single.extprecomp

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.core.Context
import com.github.lipen.satlib.core.Model
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.BooleanExpressionGuard
import ru.ifmo.fbsat.core.automaton.UnconditionalGuard
import ru.ifmo.fbsat.core.automaton.endow
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.convertBoolVarArray
import ru.ifmo.fbsat.core.solver.convertDomainVarArray
import ru.ifmo.fbsat.core.solver.convertIntVarArray
import ru.ifmo.fbsat.core.solver.solveAndGetModel
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeN_PreComp
import ru.ifmo.fbsat.core.task.single.basic.BasicTask
import ru.ifmo.fbsat.core.task.single.basic.basicMinC
import ru.ifmo.fbsat.core.utils.BinaryOperation
import ru.ifmo.fbsat.core.utils.BooleanExpression
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.UnaryOperation
import ru.ifmo.fbsat.core.utils.Variable
import ru.ifmo.fbsat.core.utils.exhaustive
import ru.ifmo.fbsat.core.utils.inputNamesPnP

private val logger = MyLogger {}

fun Inferrer.extPreComp(
    scenarioTree: PositiveScenarioTree,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    isEncodeReverseImplication: Boolean = true,
): Automaton? {
    reset()
    declare(
        BasicTask(
            scenarioTree = scenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = maxTransitions,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
    )
    declare(
        ExtPreCompTask(
            maxTotalGuardsSize = maxTotalGuardsSize
        )
    )
    return inferExtPreComp()
}

fun Inferrer.extPreCompMin(
    scenarioTree: PositiveScenarioTree,
    numberOfStates: Int? = null, // C_start
): Automaton? {
    basicMinC(scenarioTree, start = numberOfStates ?: 1) // ?: return null
    declare(ExtPreCompTask())
    return optimizeN_PreComp()
}

fun Inferrer.inferExtPreComp(): Automaton? {
    val model = solver.solveAndGetModel() ?: return null
    val automaton = buildExtPreCompAutomaton(solver.context, model)

    // solver.dumpDimacs(java.io.File("cnf"))

    // with(vars) {
    //     check(
    //         automaton.checkMapping(
    //             scenarios = scenarioTree.scenarios,
    //             mapping = assignment.mapping
    //         )
    //     ) { "Positive mapping mismatch" }
    // }

    return automaton
}

fun buildExtPreCompAutomaton(
    context: Context,
    model: Model,
): Automaton {
    val scenarioTree: PositiveScenarioTree = context["scenarioTree"]
    val uniqueInputs = scenarioTree.uniqueInputs
    val C: Int = context["C"]
    val K: Int = context["K"]
    val X: Int = context["X"]
    val Z: Int = context["Z"]
    val transitionDestination = context.convertIntVarArray("transitionDestination", model)
    val transitionInputEvent = context.convertIntVarArray("transitionInputEvent", model)
    // val transitionGuardFormula = context.convertIntVarArray("transitionGuardFormula", model)
    val guardType = context.convertDomainVarArray<GuardType>("guardType", model)
    val guardTerminalInputVariable = context.convertIntVarArray("guardTerminalInputVariable", model)
    val guardTerminalNegation = context.convertBoolVarArray("guardTerminalNegation", model)
    val stateOutputEvent = context.convertIntVarArray("stateOutputEvent", model)
    val stateAlgorithmTop = context.convertBoolVarArray("stateAlgorithmTop", model)
    val stateAlgorithmBot = context.convertBoolVarArray("stateAlgorithmBot", model)

    val vars =
        if (X == inputNamesPnP.size) {
            MultiArray.new(X) { (i) ->
                Variable(i - 1, inputNamesPnP[i - 1])
            }
        } else {
            MultiArray.new(X) { (i) ->
                Variable(i - 1, "x$i")
            }
        }

    return Automaton(scenarioTree).endow(
        C = C, K = K,
        stateOutputEvent = { c ->
            stateOutputEvent[c].let { o ->
                if (o == 0) null
                else scenarioTree.outputEvents[o - 1]
            }
        },
        stateAlgorithm = { c ->
            BinaryAlgorithm(
                algorithm0 = BooleanArray(Z) { z0 -> stateAlgorithmBot[c, z0 + 1] },
                algorithm1 = BooleanArray(Z) { z0 -> stateAlgorithmTop[c, z0 + 1] }
            )
        },
        transitionDestination = { c, k ->
            transitionDestination[c, k]
        },
        transitionInputEvent = { c, k ->
            scenarioTree.inputEvents[transitionInputEvent[c, k] - 1]
        },
        transitionGuard = { c, k ->
            fun getVarExpr(i: Int): BooleanExpression {
                val x = guardTerminalInputVariable[c, k, i]
                check(x != 0)
                return if (guardTerminalNegation[c, k, i]) {
                    UnaryOperation(
                        UnaryOperation.Kind.Not,
                        vars[x]
                    )
                } else {
                    vars[x]
                }
            }

            when (val t = guardType[c, k]) {
                GuardType.NoGuard -> error("there must be a guard")
                GuardType.True -> UnconditionalGuard()
                GuardType.Var -> {
                    val expr = getVarExpr(1)
                    BooleanExpressionGuard(expr)
                }
                GuardType.And2 -> {
                    val e1 = getVarExpr(1)
                    val e2 = getVarExpr(2)
                    val expr = BinaryOperation(
                        BinaryOperation.Kind.And,
                        e1,
                        e2
                    )
                    BooleanExpressionGuard(expr)
                }
                GuardType.Or2 -> {
                    val e1 = getVarExpr(1)
                    val e2 = getVarExpr(2)
                    val expr = BinaryOperation(
                        BinaryOperation.Kind.Or,
                        e1,
                        e2
                    )
                    BooleanExpressionGuard(expr)
                }
                GuardType.And3 -> {
                    val e1 = getVarExpr(1)
                    val e2 = getVarExpr(2)
                    val e3 = getVarExpr(3)
                    val expr = BinaryOperation(
                        BinaryOperation.Kind.And,
                        e1,
                        BinaryOperation(
                            BinaryOperation.Kind.And,
                            e2,
                            e3
                        )
                    )
                    BooleanExpressionGuard(expr)
                }
                else -> TODO()
            }.exhaustive
            // when (val f = transitionGuardFormula[c, k]) {
            //     0 -> error("transitionGuardFormula should not be zero here")
            //     1 -> UnconditionalGuard()
            //     else -> {
            //         fun getFun(): BooleanExpression {
            //             var fff = 1
            //
            //             val vars = MultiArray.new(10) { (i) ->
            //                 Variable(i - 1, inputNamesPnP[i - 1])
            //             }
            //
            //             // next X formulae: positive variable
            //             for (x in 1..X) {
            //                 fff++
            //                 if (fff == f) {
            //                     return vars[x]
            //                 }
            //             }
            //             check(fff == 1 + X)
            //
            //             // next X formulae: negative variable
            //             for (x in 1..X) {
            //                 fff++
            //                 if (fff == f) {
            //                     return UnaryOperation(
            //                         UnaryOperation.Kind.Not,
            //                         vars[x]
            //                     )
            //                 }
            //             }
            //             check(fff == 1 + 2 * X)
            //
            //             // next (X*(X-1)/2) formulae: conjunction of 2 positive variables
            //             for ((x1, x2) in (1..X).pairs()) {
            //                 fff++
            //                 if (fff == f) {
            //                     return BinaryOperation(
            //                         BinaryOperation.Kind.And,
            //                         vars[x1],
            //                         vars[x2],
            //                     )
            //                 }
            //             }
            //             check(fff == 1 + 2 * X + (X * (X - 1) / 2))
            //
            //             // next (X*(X-1)/2) formulae: conjunction of 2 negative variables
            //             for ((x1, x2) in (1..X).pairs()) {
            //                 fff++
            //                 if (fff == f) {
            //                     return BinaryOperation(
            //                         BinaryOperation.Kind.And,
            //                         UnaryOperation(
            //                             UnaryOperation.Kind.Not,
            //                             vars[x1],
            //                         ),
            //                         UnaryOperation(
            //                             UnaryOperation.Kind.Not,
            //                             vars[x2],
            //                         )
            //                     )
            //                 }
            //             }
            //             check(fff == 1 + 2 * X + 2 * (X * (X - 1) / 2))
            //
            //             // next (X*(X-1)/2) formulae: conjunction of 1 positive and 1 negative variable
            //             for ((x1, x2) in (1..X).pairs()) {
            //                 fff++
            //                 if (fff == f) {
            //                     return BinaryOperation(
            //                         BinaryOperation.Kind.And,
            //                         vars[x1],
            //                         UnaryOperation(
            //                             UnaryOperation.Kind.Not,
            //                             vars[x2],
            //                         )
            //                     )
            //                 }
            //             }
            //             check(fff == 1 + 2 * X + 3 * (X * (X - 1) / 2))
            //
            //             // next (X*(X-1)/2) formulae: conjunction of 1 negative and 1 positive variable
            //             for ((x1, x2) in (1..X).pairs()) {
            //                 fff++
            //                 if (fff == f) {
            //                     return BinaryOperation(
            //                         BinaryOperation.Kind.And,
            //                         UnaryOperation(
            //                             UnaryOperation.Kind.Not,
            //                             vars[x1],
            //                         ),
            //                         vars[x2],
            //                     )
            //                 }
            //             }
            //             check(fff == 1 + 2 * X + 4 * (X * (X - 1) / 2))
            //
            //             // next (X*(X-1)/2) formulae: disjunction of 2 positive variables
            //             for ((x1, x2) in (1..X).pairs()) {
            //                 fff++
            //                 if (fff == f) {
            //                     return BinaryOperation(
            //                         BinaryOperation.Kind.Or,
            //                         vars[x1],
            //                         vars[x2],
            //                     )
            //                 }
            //             }
            //             check(fff == 1 + 2 * X + 5 * (X * (X - 1) / 2))
            //
            //             // next (X*(X-1)*(X-2)/6) formulae: conjunction of 3 positive variables
            //             for ((x1, x2, x3) in (1..X).triples()) {
            //                 fff++
            //                 if (fff == f) {
            //                     return BinaryOperation(
            //                         BinaryOperation.Kind.And,
            //                         vars[x1],
            //                         BinaryOperation(
            //                             BinaryOperation.Kind.And,
            //                             vars[x2],
            //                             vars[x3],
            //                         )
            //                     )
            //                 }
            //             }
            //             check(fff == 1 + 2 * X + 5 * (X * (X - 1) / 2) + (X * (X - 1) * (X - 2) / 6))
            //
            //             error("reached F")
            //         }
            //
            //         val expr = getFun()
            //         BooleanExpressionGuard(expr)
            //     }
            // }
        }
    )
}
