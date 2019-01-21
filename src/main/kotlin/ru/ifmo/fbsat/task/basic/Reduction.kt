package ru.ifmo.fbsat.task.basic

import ru.ifmo.fbsat.scenario.CounterExampleTree
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.solver.atMostOne
import ru.ifmo.fbsat.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.solver.exactlyOne
import ru.ifmo.fbsat.solver.iff
import ru.ifmo.fbsat.solver.iffAnd
import ru.ifmo.fbsat.solver.iffOr
import ru.ifmo.fbsat.solver.imply
import ru.ifmo.fbsat.solver.declareTotalizer
import ru.ifmo.fbsat.utils.IntMultiArray

/**
 * BASIC method
 */
internal class Reduction(
    val scenarioTree: ScenarioTree,
    val C: Int,
    val K: Int,
    val color: IntMultiArray,
    val transition: IntMultiArray,
    val outputEvent: IntMultiArray,
    val algorithm0: IntMultiArray,
    val algorithm1: IntMultiArray,
    val firstFired: IntMultiArray,
    val notFired: IntMultiArray
) {
    companion object {
        @Suppress("LocalVariableName")
        fun declareBaseReduction(solver: Solver, scenarioTree: ScenarioTree, C: Int, K: Int): Reduction {
            // Constants
            val V = scenarioTree.size
            val E = scenarioTree.inputEvents.size
            val O = scenarioTree.outputEvents.size
            val U = scenarioTree.uniqueInputs.size
            val Z = scenarioTree.uniqueOutputs.first().length
            // Automaton variables
            val color = solver.newArray(V, C)
            val transition = solver.newArray(C, E, K, C + 1)
            val outputEvent = solver.newArray(C, O)
            val algorithm0 = solver.newArray(C, Z)
            val algorithm1 = solver.newArray(C, Z)
            // Guards variables
            val firstFired = solver.newArray(C, E, U, K)
            val notFired = solver.newArray(C, E, U, K)
            // BFS variables
            val bfs_transition = solver.newArray(C, C)
            val bfs_parent = solver.newArray(C, C)

            solver.comment("1. Color constraints")
            solver.comment("1.0. ALO/AMO(color)")
            for (v in 1..V)
                solver.exactlyOne(1..C, color, v)

            solver.comment("1.1. Start vertex corresponds to start state")
            solver.clause(color[1, 1])

            solver.comment("1.2. Color definition")
            // color[v, c] => color[tp(v), c]
            for (c in 1..C)
                for (v in scenarioTree.passiveVertices)
                    solver.iff(color[v, c], color[scenarioTree.parent(v), c])

            solver.comment("2. Transition constraints")
            solver.comment("2.0. ALO/AMO(transition)")
            for (c in 1..C)
                for (e in 1..E)
                    for (k in 1..K)
                        solver.exactlyOne(1..(C + 1), transition, c, e, k)

            solver.comment("2.1. (transition + first_fired definitions)")
            // (color[tp(v),i] & color[v,j]) => OR_k( transition[i,tie(v),k,j] & first_fired[i,tie(v),tin(v),k] )
            for (v in scenarioTree.activeVertices) {
                val p = scenarioTree.parent(v)
                val e = scenarioTree.inputEvent(v)
                val u = scenarioTree.inputNumber(v)
                for (i in 1..C)
                    for (j in 1..C) {
                        val rhs = sequence {
                            for (k in 1..K) {
                                val aux = solver.newVariable()
                                solver.iffAnd(aux, transition[i, e, k, j], firstFired[i, e, u, k])
                                yield(aux)
                            }
                        }
                        val right = solver.newVariable()
                        solver.iffOr(right, rhs)
                        solver.clause(-color[v, j], -color[p, i], right)
                    }
            }

            solver.comment("2.2. Null-transitions are last")
            // transition[k, 0] => transition[k+1, 0]
            for (c in 1..C)
                for (e in 1..E)
                    for (k in 1..(K - 1))
                        solver.imply(transition[c, e, k, C + 1], transition[c, e, k + 1, C + 1])

            solver.comment("3. Firing constraints")
            solver.comment("3.0. only AMO(first_fired)")
            for (c in 1..C)
                for (e in 1..E)
                    for (u in 1..U)
                        solver.atMostOne(1..K, firstFired, c, e, u)

            solver.comment("3.1. (not_fired definition)")
            // not_fired[c,e,u,K] <=> OR_{v|passive,tie(v)=e,tin(v)=u}(color[v,c])
            for (c in 1..C)
                for (e in 1..E)
                    for (u in 1..U) {
                        val rhs = sequence {
                            for (v in scenarioTree.passiveVerticesEU.getOrElse(e to u) { listOf() }) {
                                yield(color[v, c])
                            }
                        }
                        solver.iffOr(notFired[c, e, u, K], rhs)
                    }

            solver.comment("3.2. not_fired extension")
            for (c in 1..C)
                for (e in 1..E)
                    for (u in 1..U) {
                        // nf_k => nf_{k-1}
                        for (k in 2..K)
                            solver.imply(notFired[c, e, u, k], notFired[c, e, u, k - 1])
                        // ~nf_k => ~nf_{k+1}
                        for (k in 1..(K - 1))
                            solver.imply(-notFired[c, e, u, k], -notFired[c, e, u, k + 1])
                    }

            solver.comment("3.3. first_fired and not_fired interaction")
            for (c in 1..C)
                for (e in 1..E)
                    for (u in 1..U) {
                        // ~(ff & nf)
                        for (k in 1..K)
                            solver.clause(-firstFired[c, e, u, k], -notFired[c, e, u, k])
                        // ff_k => nf_{k-1}
                        for (k in 2..K)
                            solver.imply(firstFired[c, e, u, k], notFired[c, e, u, k - 1])
                    }

            solver.comment("4. Output event constraints")
            solver.comment("4.0. ALO/AMO(output_event)")
            for (c in 1..C)
                solver.exactlyOne(1..O, outputEvent, c)

            solver.comment("4.1. Start state does INITO (root`s output event)")
            solver.clause(outputEvent[1, scenarioTree.outputEvent(1)])

            solver.comment("4.2. Output event is the same as in the tree")
            // OR_{e,k}(transition[i,e,k,j]) <=> ...
            // ... <=> OR_{v|active}( color[tp(v), i] & color[v, j] & output_event[j, toe(v)] )
            for (i in 1..C)
                for (j in 1..C) {
                    val leftright = solver.newVariable()

                    val lhs = sequence {
                        for (e in 1..E)
                            for (k in 1..K)
                                yield(transition[i, e, k, j])
                    }
                    solver.iffOr(leftright, lhs)

                    val rhs = sequence {
                        for (v in scenarioTree.activeVertices) {
                            // aux <=> color[tp(v),i] & color[v,j] & output_event[j,toe(v)]
                            val aux = solver.newVariable()
                            val p = scenarioTree.parent(v)
                            val o = scenarioTree.outputEvent(v)
                            solver.iffAnd(aux, color[p, i], color[v, j], outputEvent[j, o])
                            yield(aux)
                        }
                    }
                    solver.iffOr(leftright, rhs)
                }

            solver.comment("5. Algorithm constraints")
            solver.comment("5.1. Start state does nothing")
            for (z in 1..Z) {
                solver.clause(-algorithm0[1, z])
                solver.clause(algorithm1[1, z])
            }

            solver.comment("5.2. Algorithms definition")
            for (v in scenarioTree.activeVertices)
                for (z in 1..Z) {
                    val oldValue = scenarioTree.outputValue(scenarioTree.parent(v), z)
                    val newValue = scenarioTree.outputValue(v, z)
                    for (c in 1..C)
                        solver.imply(
                            lhs = color[v, c],
                            rhs = when (val values = oldValue to newValue) {
                                false to false -> -algorithm0[c, z]
                                false to true -> algorithm0[c, z]
                                true to false -> -algorithm1[c, z]
                                true to true -> algorithm1[c, z]
                                else -> throw Exception("Weird combination of values: $values")
                            }
                        )
                }

            solver.comment("6. BFS constraints")
            solver.comment("6.1. F_t")
            // t[i, j] <=> OR_{e,k}( transition[i,e,k,j] )
            for (i in 1..C)
                for (j in 1..C)
                    solver.iffOr(bfs_transition[i, j],
                        sequence {
                            for (e in 1..E)
                                for (k in 1..K)
                                    yield(transition[i, e, k, j])
                        })

            solver.comment("6.2. F_p")
            // p[j, i] <=> t[i, j] & AND_{k<i}( ~t[k, j] )
            for (i in 1..C) {
                // to avoid ambiguous unused variable:
                for (j in 1..i)
                    solver.clause(-bfs_parent[j, i])

                for (j in (i + 1)..C)
                    solver.iffAnd(bfs_parent[j, i],
                        sequence {
                            yield(bfs_transition[i, j])
                            for (k in 1..(i - 1))
                                yield(-bfs_transition[k, j])
                        })
            }

            solver.comment("6.3. F_ALO(p)")
            for (j in 2..C)
                solver.clause(sequence {
                    for (i in 1..(j - 1))
                        yield(bfs_parent[j, i])
                })

            solver.comment("6.4. F_BFS(p)")
            // p[j, i] => ~p[j+1, k]
            for (k in 1..C)
                for (i in (k + 1)..C)
                    for (j in (i + 1)..(C - 1))
                        solver.imply(bfs_parent[j, i], -bfs_parent[j + 1, k])

            solver.comment("A. AD-HOCs")
            solver.comment("A.1. Distinct transitions")
            // TODO: Distinct transitions

            return Reduction(
                scenarioTree, C, K,
                color, transition, outputEvent, algorithm0, algorithm1,
                firstFired, notFired
            )
        }

        fun declareTotalizer(solver: Solver, baseReduction: Reduction): IntArray {
            return solver.declareTotalizer(sequence {
                val (C, E, K, _) = baseReduction.transition.shape
                for (c in 1..C)
                    for (e in 1..E)
                        for (k in 1..K)
                            yield(-baseReduction.transition[c, e, k, C + 1])
            })
        }

        fun declareComparator(solver: Solver, totalizer: IntArray, T: Int, declaredT: Int? = null) {
            solver.declareComparatorLessThanOrEqual(totalizer, T, declaredT)
        }

        @Suppress("LocalVariableName")
        fun declareCE(
            solver: Solver,
            baseReduction: Reduction,
            counterExampleTree: CounterExampleTree
        ) {
            // Constants
            val C = baseReduction.C
            val K = baseReduction.K
            val V = counterExampleTree.size
            val E = counterExampleTree.inputEvents.size
            val O = counterExampleTree.outputEvents.size
            val U = counterExampleTree.uniqueInputs.size
            val Z = counterExampleTree.uniqueOutputs.first().length
            // Automaton variables
            val color = baseReduction.color
            val transition = baseReduction.transition
            val outputEvent = baseReduction.outputEvent
            val algorithm0 = baseReduction.algorithm0
            val algorithm1 = baseReduction.algorithm1
            // Guards variables
            val firstFired = baseReduction.firstFired
            val notFired = baseReduction.notFired
            // Color propagation variable
            val prop = solver.newArray(V)

            solver.comment("[NEGATIVE] Color propagation on active vertices")
            // color[v,j] & color[tp(v),i] & ...
            // ...OR_k( transition[i,tie(v),k,j] & first_fired[i,tie(v),tin(v),k & output_event[j,toe(v)] )...
            // ...& prop[tp(v)] => prop[v]
            for (v in counterExampleTree.activeVertices) {
                val p = counterExampleTree.parent(v)
                val e = counterExampleTree.inputEvent(v)
                val u = counterExampleTree.inputNumber(v)
                val o = counterExampleTree.outputEvent(v)
                for (i in 1..C)
                    for (j in 1..C) {
                        val t = solver.newVariable()
                        val ts = sequence {
                            for (k in 1..K) {
                                val aux = solver.newVariable()
                                solver.iffAnd(
                                    aux,
                                    transition[i, e, k, j],
                                    firstFired[i, e, u, k],
                                    outputEvent[j, o]
                                )
                                yield(aux)
                            }
                        }
                        solver.iffOr(t, ts)

                        val lhs = solver.newVariable()
                        solver.iffAnd(lhs, color[v, j], color[p, i], t, prop[p])

                        solver.imply(lhs, prop[v])
                    }
            }

            solver.comment("[NEGATIVE] Color propagation on passive vertices")
            // color[v,c] & AND_k(transition[c,tie(v),k,0]) & prop[tp(v)] => prop[v]
            for (v in counterExampleTree.passiveVertices) {
                val p = counterExampleTree.parent(v)
                val e = counterExampleTree.inputEvent(v)
                for (c in 1..C) {
                    val nts = sequence {
                        for (k in 1..K) {
                            yield(-transition[c, e, k, C + 1])
                        }
                    }.toList().toIntArray()
                    solver.clause(-color[v, c], *nts, -prop[p], prop[v])
                }
            }

            solver.comment("[NEGATIVE] Color non-propagation")
            for (v in 2..V) {
                val p = counterExampleTree.parent(v)
                solver.imply(-prop[p], -prop[v])
            }

            solver.comment("[NEGATIVE] Forbid same color of loop start/end")
            // prop[v] & prop[loop(v)] => AND_c( color[v,c] => ~color[loop(v),c] )
            for (v in counterExampleTree.verticesWithLoops) {
                val l = counterExampleTree.loopBack(v)
                for (c in 1..C) {
                    solver.clause(-prop[v], -prop[l], -color[v, c], -color[l, c])
                }
            }
        }
    }
}
