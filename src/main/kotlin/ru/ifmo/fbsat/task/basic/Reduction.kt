package ru.ifmo.fbsat.task.basic

import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.solver.atMostOne
import ru.ifmo.fbsat.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.solver.declareTotalizer
import ru.ifmo.fbsat.solver.exactlyOne
import ru.ifmo.fbsat.solver.iff
import ru.ifmo.fbsat.solver.iffAnd
import ru.ifmo.fbsat.solver.iffOr
import ru.ifmo.fbsat.solver.imply
import ru.ifmo.multiarray.IntMultiArray

class Reduction(
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
)

@Suppress("LocalVariableName")
fun Solver.declareBaseReduction(scenarioTree: ScenarioTree, C: Int, K: Int): Reduction {
    // Constants
    val V = scenarioTree.size
    val E = scenarioTree.inputEvents.size
    val O = scenarioTree.outputEvents.size
    val U = scenarioTree.uniqueInputs.size
    val Z = scenarioTree.uniqueOutputs.first().length
    // Automaton variables
    val color = newArray(V, C)
    val transition = newArray(C, E, K, C + 1)
    val outputEvent = newArray(C, O)
    val algorithm0 = newArray(C, Z)
    val algorithm1 = newArray(C, Z)
    // Guards variables
    val firstFired = newArray(C, E, U, K)
    val notFired = newArray(C, E, U, K)
    // BFS variables
    val bfs_transition = newArray(C, C)
    val bfs_parent = newArray(C, C)

    comment("1. Color constraints")
    comment("1.0. ALO/AMO(color)")
    for (v in 1..V)
        exactlyOne(1..C, color, v)

    comment("1.1. Start vertex corresponds to start state")
    clause(color[1, 1])

    comment("1.2. Color definition")
    // color[v, c] => color[tp(v), c]
    for (c in 1..C)
        for (v in scenarioTree.passiveVertices)
            iff(color[v, c], color[scenarioTree.parent(v), c])

    comment("2. Transition constraints")
    comment("2.0. ALO/AMO(transition)")
    for (c in 1..C)
        for (e in 1..E)
            for (k in 1..K)
                exactlyOne(1..(C + 1), transition, c, e, k)

    comment("2.1. (transition + first_fired definitions)")
    // (color[tp(v),i] & color[v,j]) => OR_k( transition[i,tie(v),k,j] & first_fired[i,tie(v),tin(v),k] )
    for (v in scenarioTree.activeVertices) {
        val p = scenarioTree.parent(v)
        val e = scenarioTree.inputEvent(v)
        val u = scenarioTree.inputNumber(v)
        for (i in 1..C)
            for (j in 1..C) {
                val rhs = sequence {
                    for (k in 1..K) {
                        val aux = newVariable()
                        iffAnd(aux, transition[i, e, k, j], firstFired[i, e, u, k])
                        yield(aux)
                    }
                }
                val right = newVariable()
                iffOr(right, rhs)
                clause(-color[v, j], -color[p, i], right)
            }
    }

    comment("2.2. Null-transitions are last")
    // transition[k, 0] => transition[k+1, 0]
    for (c in 1..C)
        for (e in 1..E)
            for (k in 1..(K - 1))
                imply(transition[c, e, k, C + 1], transition[c, e, k + 1, C + 1])

    comment("3. Firing constraints")
    comment("3.0. only AMO(first_fired)")
    for (c in 1..C)
        for (e in 1..E)
            for (u in 1..U)
                atMostOne(1..K, firstFired, c, e, u)

    comment("3.1. (not_fired definition)")
    // not_fired[c,e,u,K] <=> OR_{v|passive,tie(v)=e,tin(v)=u}(color[v,c])
    for (c in 1..C)
        for (e in 1..E)
            for (u in 1..U) {
                val rhs = sequence {
                    for (v in scenarioTree.passiveVerticesEU.getOrElse(e to u) { listOf() }) {
                        yield(color[v, c])
                    }
                }
                iffOr(notFired[c, e, u, K], rhs)
            }

    comment("3.2. not_fired extension")
    for (c in 1..C)
        for (e in 1..E)
            for (u in 1..U) {
                // nf_k => nf_{k-1}
                for (k in 2..K)
                    imply(notFired[c, e, u, k], notFired[c, e, u, k - 1])
                // ~nf_k => ~nf_{k+1}
                for (k in 1..(K - 1))
                    imply(-notFired[c, e, u, k], -notFired[c, e, u, k + 1])
            }

    comment("3.3. first_fired and not_fired interaction")
    for (c in 1..C)
        for (e in 1..E)
            for (u in 1..U) {
                // ~(ff & nf)
                for (k in 1..K)
                    clause(-firstFired[c, e, u, k], -notFired[c, e, u, k])
                // ff_k => nf_{k-1}
                for (k in 2..K)
                    imply(firstFired[c, e, u, k], notFired[c, e, u, k - 1])
            }

    comment("4. Output event constraints")
    comment("4.0. ALO/AMO(output_event)")
    for (c in 1..C)
        exactlyOne(1..O, outputEvent, c)

    comment("4.1. Start state does INITO (root`s output event)")
    clause(outputEvent[1, scenarioTree.outputEvent(1)])

    comment("4.2. Output event is the same as in the tree")
    // OR_{e,k}(transition[i,e,k,j]) <=> ...
    // ... <=> OR_{v|active}( color[tp(v), i] & color[v, j] & output_event[j, toe(v)] )
    for (i in 1..C)
        for (j in 1..C) {
            val leftright = newVariable()

            val lhs = sequence {
                for (e in 1..E)
                    for (k in 1..K)
                        yield(transition[i, e, k, j])
            }
            iffOr(leftright, lhs)

            val rhs = sequence {
                for (v in scenarioTree.activeVertices) {
                    // aux <=> color[tp(v),i] & color[v,j] & output_event[j,toe(v)]
                    val aux = newVariable()
                    val p = scenarioTree.parent(v)
                    val o = scenarioTree.outputEvent(v)
                    iffAnd(aux, color[p, i], color[v, j], outputEvent[j, o])
                    yield(aux)
                }
            }
            iffOr(leftright, rhs)
        }

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
                    lhs = color[v, c],
                    rhs = when (val values = oldValue to newValue) {
                        false to false -> -algorithm0[c, z]
                        false to true -> algorithm0[c, z]
                        true to false -> -algorithm1[c, z]
                        true to true -> algorithm1[c, z]
                        else -> error("Weird combination of values: $values")
                    }
                )
        }

    comment("6. BFS constraints")
    comment("6.1. F_t")
    // t[i, j] <=> OR_{e,k}( transition[i,e,k,j] )
    for (i in 1..C)
        for (j in 1..C)
            iffOr(bfs_transition[i, j],
                sequence {
                    for (e in 1..E)
                        for (k in 1..K)
                            yield(transition[i, e, k, j])
                })

    comment("6.2. F_p")
    // p[j, i] <=> t[i, j] & AND_{k<i}( ~t[k, j] )
    for (i in 1..C) {
        // to avoid ambiguous unused variable:
        for (j in 1..i)
            clause(-bfs_parent[j, i])

        for (j in (i + 1)..C)
            iffAnd(bfs_parent[j, i],
                sequence {
                    yield(bfs_transition[i, j])
                    for (k in 1..(i - 1))
                        yield(-bfs_transition[k, j])
                })
    }

    comment("6.3. F_ALO(p)")
    for (j in 2..C)
        clause(sequence {
            for (i in 1..(j - 1))
                yield(bfs_parent[j, i])
        })

    comment("6.4. F_BFS(p)")
    // p[j, i] => ~p[j+1, k]
    for (k in 1..C)
        for (i in (k + 1)..C)
            for (j in (i + 1)..(C - 1))
                imply(bfs_parent[j, i], -bfs_parent[j + 1, k])

    comment("A. AD-HOCs")
    comment("A.1. Distinct transitions")
    // TODO: Distinct transitions

    return Reduction(
        scenarioTree, C, K,
        color, transition, outputEvent, algorithm0, algorithm1,
        firstFired, notFired
    )
}

fun Solver.declareTotalizer(baseReduction: Reduction): IntArray {
    return declareTotalizer(sequence {
        val (C, E, K, _) = baseReduction.transition.shape
        for (c in 1..C)
            for (e in 1..E)
                for (k in 1..K)
                    yield(-baseReduction.transition[c, e, k, C + 1])
    })
}

fun Solver.declareComparator(totalizer: IntArray, T: Int, declaredT: Int? = null) {
    declareComparatorLessThanOrEqual(totalizer, T, declaredT)
}
