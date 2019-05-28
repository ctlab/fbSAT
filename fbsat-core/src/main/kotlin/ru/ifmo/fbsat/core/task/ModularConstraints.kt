@file:Suppress("LocalVariableName", "ReplaceRangeToWithUntil")

package ru.ifmo.fbsat.core.task

import com.github.lipen.multiarray.IntMultiArray
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.atLeastOne
import ru.ifmo.fbsat.core.solver.exactlyOne
import ru.ifmo.fbsat.core.solver.iff
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffOr
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.implyAnd

fun Solver.declareModularColorConstraints(isEncodeReverseImplication: Boolean = true) {
    val scenarioTree: ScenarioTree by context
    val M: Int by context
    val C: Int by context
    val K: Int by context
    val V: Int by context
    val color: IntMultiArray by context
    val transition: IntMultiArray by context
    val actualTransition: IntMultiArray by context

    comment("1. Color constraints")

    comment("1.0. ONE(color)_{1..C}")
    for (m in 1..M)
        for (v in 1..V)
            exactlyOne {
                for (c in 1..C)
                    yield(color[m, v, c])
            }

    comment("1.1. Color of active vertices")
    // color[tp(v), i] & color[v, j] => actual_transition[i,tie(v),tin(v),j]
    for (m in 1..M)
        for (v in scenarioTree.activeVertices) {
            val p = scenarioTree.parent(v)
            val e = scenarioTree.inputEvent(v)
            val u = scenarioTree.inputNumber(v)
            for (i in 1..C)
                for (j in 1..C)
                    clause(
                        -color[m, p, i],
                        -color[m, v, j],
                        actualTransition[m, i, e, u, j]
                    )
        }
    if (isEncodeReverseImplication) {
        // OR_k(transition[i,k,j]) <=> OR_{v|active}( color[tp(v), i] & color[v, j] )
        for (m in 1..M)
            for (i in 1..C)
                for (j in 1..C) {
                    val lhsAux = newVariable()
                    iffOr(lhsAux, sequence {
                        for (k in 1..K)
                            yield(transition[m, i, k, j])
                    })

                    val rhsAux = newVariable()
                    iffOr(rhsAux, sequence {
                        for (v in scenarioTree.activeVertices) {
                            val p = scenarioTree.parent(v)
                            val aux = newVariable()
                            iffAnd(aux, color[m, p, i], color[m, v, j])
                            yield(aux)
                        }
                    })

                    imply(lhsAux, rhsAux)
                }
    }
    comment("1.2. Color of passive vertices")
    // color[tp(v), c] => actual_transition[c,tie(v),tin(v),0]
    for (m in 1..M)
        for (v in scenarioTree.passiveVertices) {
            val p = scenarioTree.parent(v)
            val e = scenarioTree.inputEvent(v)
            val u = scenarioTree.inputNumber(v)
            for (c in 1..C)
                imply(color[m, p, c], actualTransition[m, c, e, u, C + 1])
        }

    comment("1.3. Color propagation for passive vertices")
    // color[tp(v), c] => color[v, c]
    for (m in 1..M)
        for (v in scenarioTree.passiveVertices) {
            val p = scenarioTree.parent(v)
            for (c in 1..C)
                imply(color[m, p, c], color[m, v, c])
        }

    comment("1.4. Root corresponds to start state")
    for (m in 1..M)
        clause(color[m, 1, 1])
}

fun Solver.declareModularTransitionConstraints() {
    val M: Int by context
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
    for (m in 1..M)
        for (i in 1..C)
            for (k in 1..K)
                exactlyOne {
                    for (j in 1..(C + 1))
                        yield(transition[m, i, k, j])
                }

    comment("2.0b. ONE(actual_transition)_{0..C}")
    for (m in 1..M)
        for (i in 1..C)
            for (e in 1..E)
                for (u in 1..U)
                    exactlyOne {
                        for (j in 1..(C + 1))
                            yield(actualTransition[m, i, e, u, j])
                    }

    comment("2.0c. ONE(input_event)_{0..E}")
    for (m in 1..M)
        for (c in 1..C)
            for (k in 1..K)
                exactlyOne {
                    for (e in 1..(E + 1))
                        yield(inputEvent[m, c, k, e])
                }

    comment("2.1. Active transition definition")
    // actual_transition[i,e,u,j] <=> OR_k( transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k] )
    for (m in 1..M)
        for (i in 1..C)
            for (e in 1..E)
                for (u in 1..U)
                    for (j in 1..C)
                        iffOr(actualTransition[m, i, e, u, j], sequence {
                            for (k in 1..K) {
                                // aux <=> transition[i,k,j] & input_event[i,k,e] & first_fired[i,u,k]
                                val aux = newVariable()
                                iffAnd(
                                    aux,
                                    transition[m, i, k, j],
                                    inputEvent[m, i, k, e],
                                    firstFired[m, i, u, k]
                                )
                                yield(aux)
                            }
                        })

    comment("2.2. Null-transitions are last")
    // transition[k, 0] => transition[k+1, 0]
    for (m in 1..M)
        for (c in 1..C)
            for (k in 1 until K)
                imply(transition[m, c, k, C + 1], transition[m, c, k + 1, C + 1])

    comment("2.3. Only null-transitions have no input event")
    // transition[k, 0] <=> input_event[k, 0]
    for (m in 1..M)
        for (c in 1..C)
            for (k in 1..K)
                iff(transition[m, c, k, C + 1], inputEvent[m, c, k, E + 1])

    comment("+2.4. Ad-hoc: no transition to the first state")
    for (m in 1..M)
        for (c in 1..C)
            for (k in 1..K)
                clause(-transition[m, c, k, 1])
}

fun Solver.declareModularFiringConstraints() {
    val M: Int by context
    val C: Int by context
    val K: Int by context
    val U: Int by context
    val rootValue: IntMultiArray by context
    val firstFired: IntMultiArray by context
    val notFired: IntMultiArray by context

    comment("3. Firing constraints")

    comment("3.0. ONE(first_fired)_{0..K}")
    for (m in 1..M)
        for (c in 1..C)
            for (u in 1..U)
                exactlyOne {
                    for (k in 1..(K + 1))
                        yield(firstFired[m, c, u, k])
                }

    comment("3.1. first_fired definition")
    // first_fired[k] <=> root_value[k] & not_fired[k-1]
    for (m in 1..M)
        for (c in 1..C)
            for (u in 1..U) {
                iff(firstFired[m, c, u, 1], rootValue[m, c, 1, u])
                for (k in 2..K)
                    iffAnd(firstFired[m, c, u, k], rootValue[m, c, k, u], notFired[m, c, u, k - 1])
            }

    comment("3.2. not_fired definition")
    // not_fired[k] <=> ~root_value[k] & not_fired[k-1]
    for (m in 1..M)
        for (c in 1..C)
            for (u in 1..U) {
                iff(notFired[m, c, u, 1], -rootValue[m, c, 1, u])
                for (k in 2..K)
                    iffAnd(notFired[m, c, u, k], -rootValue[m, c, k, u], notFired[m, c, u, k - 1])
            }

    comment("3.3. Propagation of not-not_fired (maybe redundant)")
    // ~not_fired[k] => ~not_fired[k+1]
    for (m in 1..M)
        for (c in 1..C)
            for (u in 1..U)
                for (k in 1 until K)
                    imply(-notFired[m, c, u, k], -notFired[m, c, u, k + 1])

    comment("3.4. first_fired[0] <=> not_fired[K] (shortcut)")
    // first_fired[0] <=> not_fired[K]
    for (m in 1..M)
        for (c in 1..C)
            for (u in 1..U)
                iff(firstFired[m, c, u, K + 1], notFired[m, c, u, K])
}

fun Solver.declareModularOutputEventConstraints() {
    val scenarioTree: ScenarioTree by context
    val M: Int by context
    val C: Int by context
    val O: Int by context
    val color: IntMultiArray by context
    val outputEvent: IntMultiArray by context

    comment("4. Output event constraints")

    comment("4.0. ONE(output_event)_{1..O}")
    for (m in 1..M)
        for (c in 1..C)
            exactlyOne {
                for (o in 1..O)
                    yield(outputEvent[m, c, o])
            }

    comment("4.1. output_event definition")
    // color[v, c] => output_event[c, toe(v)]
    for (m in 1..M)
        for (v in scenarioTree.activeVertices) {
            val o = scenarioTree.outputEvent(v)
            for (c in 1..C)
                imply(color[m, v, c], outputEvent[m, c, o])
        }

    comment("4.2. Start state does INITO (root's output event)")
    for (m in 1..M)
        clause(outputEvent[m, 1, scenarioTree.outputEvent(1)])
}

fun Solver.declareModularAlgorithmConstraints() {
    val scenarioTree: ScenarioTree by context
    val M: Int by context
    val C: Int by context
    val Z: Int by context
    val outputVariableModule: IntMultiArray by context
    val algorithm0: IntMultiArray by context
    val algorithm1: IntMultiArray by context
    val color: IntMultiArray by context

    // EO
    for (z in 1..Z)
        exactlyOne {
            for (m in 1..M)
                yield(outputVariableModule[z, m])
        }
    // ALO
    for (m in 1..M)
        atLeastOne {
            for (z in 1..Z)
                yield(outputVariableModule[z, m])
        }

    comment("5. Algorithm constraints")

    comment("5.1. Start state produces zero outputs")
    for (m in 1..M)
        for (z in 1..Z) {
            clause(-algorithm0[m, 1, z])
            clause(-algorithm1[m, 1, z])
        }

    comment("5.2. Algorithms definition")
    for (v in scenarioTree.activeVertices) {
        val p = scenarioTree.parent(v)
        for (z in 1..Z) {
            val oldValue = scenarioTree.outputValue(p, z)
            val newValue = scenarioTree.outputValue(v, z)
            for (m in 1..M)
                for (c in 1..C)
                    clause(
                        -outputVariableModule[z, m],
                        -color[m, v, c],
                        when (val values = oldValue to newValue) {
                            false to false -> -algorithm0[m, c, z]
                            false to true -> algorithm0[m, c, z]
                            true to false -> -algorithm1[m, c, z]
                            true to true -> algorithm1[m, c, z]
                            else -> error("Weird combination of values: $values")
                        }
                    )
        }
    }

    for (m in 1..M)
        for (z in 1..Z)
            for (c in 1..C) {
                implyAnd(-outputVariableModule[z, m], -algorithm0[m, c, z])
                implyAnd(-outputVariableModule[z, m], -algorithm1[m, c, z])
            }
}
