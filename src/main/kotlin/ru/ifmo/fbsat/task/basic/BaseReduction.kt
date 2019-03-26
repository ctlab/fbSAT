package ru.ifmo.fbsat.task.basic

import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.solver.exactlyOne
import ru.ifmo.fbsat.solver.iff
import ru.ifmo.fbsat.solver.iffAnd
import ru.ifmo.fbsat.solver.iffOr
import ru.ifmo.fbsat.solver.imply
import ru.ifmo.multiarray.IntMultiArray

internal class BaseReduction(
    val scenarioTree: ScenarioTree,
    val C: Int,
    val K: Int,
    solver: Solver,
    isEncodeTransitionsOrder: Boolean
) {
    // Constants
    private val V = scenarioTree.size
    private val E = scenarioTree.inputEvents.size
    private val O = scenarioTree.outputEvents.size
    private val U = scenarioTree.uniqueInputs.size
    private val X = scenarioTree.uniqueInputs.first().length
    private val Z = scenarioTree.uniqueOutputs.first().length
    // Scenario tree variables
    val color: IntMultiArray // [V, C]
    // Automaton variables
    val transition: IntMultiArray // [C, K, C+1]
    val actualTransition: IntMultiArray // [C, E, U, C+1]
    val inputEvent: IntMultiArray // [C, K, E+1]
    val outputEvent: IntMultiArray // [C, O]
    val algorithm0: IntMultiArray // [C, Z]
    val algorithm1: IntMultiArray // [C, Z]
    // Guards variables
    val rootValue: IntMultiArray // [C, K, U]
    val firstFired: IntMultiArray // [C, U, K+1]
    val notFired: IntMultiArray // [C, U, K]
    // BFS variables
    val bfsTransition: IntMultiArray // [C, C]
    val bfsParent: IntMultiArray // [C, C]

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
            rootValue = newArray(C, K, U)
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
            declareAdhocConstraints()
            if (isEncodeTransitionsOrder) declareTransitionsOrderConstraints()
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
            for (k in 1..K)
                clause(-transition[c, k, 1])
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

        comment("5.1. Start state produces zero outputs")
        for (z in 1..Z) {
            clause(-algorithm0[1, z])
            clause(-algorithm1[1, z])
        }

        comment("5.2. Algorithms definition")
        for (v in scenarioTree.activeVertices) {
            val p = scenarioTree.parent(v)
            for (z in 1..Z) {
                val oldValue = scenarioTree.outputValue(p, z)
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

    private fun Solver.declareAdhocConstraints() {
        comment("A. AD-HOCs")

        comment("A.2. Distinct transitions")
        // TODO: Distinct transitions
    }

    private fun Solver.declareTransitionsOrderConstraints() {
        comment("+++. Transitions order constraints")

        // transition[i,k,j] => AND_{k'<k, j'>j}( ~transition[i,k',j'] )
        for (i in 1..C)
            for (k in 2..K)
                for (k_ in 1 until k)
                    for (j in 1..(C - 1))
                        for (j_ in (j + 1)..C)
                            imply(
                                transition[i, k, j],
                                -transition[i, k_, j_]
                            )

        // transition[i,k,j] => AND_{k'<k}( OR_{j'<=j}( transition[i,k',j'] ) )
        for (i in 1..C)
            for (k in 2..K)
                for (k_ in 1 until k)
                    for (j in 1..C)
                        clause {
                            yield(-transition[i, k, j])
                            for (j_ in 1..j)
                                yield(transition[i, k_, j_])
                        }
    }
}
