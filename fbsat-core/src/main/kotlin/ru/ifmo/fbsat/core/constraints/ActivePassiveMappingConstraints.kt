package ru.ifmo.fbsat.core.constraints

import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.eq
import com.github.lipen.satlib.core.neq
import com.github.lipen.satlib.op.iffAnd
import com.github.lipen.satlib.op.iffOr
import com.github.lipen.satlib.op.imply
import com.github.lipen.satlib.op.implyIff
import com.github.lipen.satlib.op.implyImplyIff
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.imply2
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.algorithmChoice

// Standard reduction:
// fbsat infer extended-min -P 5 -i data\tests-39.gz --glucose --debug
// [18:28:08] [I] BasicTask: declared 63900 variables and 326593 clauses in 0.462 s.
// [18:28:09] [I] BasicMin: C = 8 -> SAT in 1.416 s.
// [18:28:09] [D] Propagations: 11089340
// [18:28:09] [D] Conflicts: 2380
// [18:28:09] [D] Decisions: 40045
// [18:28:09] [I] BasicMin: minimal C = 8
// [18:28:09] [I] ExtendedTask: declared 19968 variables and 424128 clauses in 0.365 s.
// [18:28:21] [I] All done in 17.649 seconds
//
// New active-passive reduction: (278bbaf9ac1d6527c135785124f3f508fdef7103)
// fbsat infer extended-min -P 5 -i data\tests-39.gz --glucose --debug --encode-active-passive
// [18:26:50] [I] BasicTask: declared 107932 variables and 827352 clauses in 0.821 s.
// [18:26:51] [I] BasicMin: C = 8 -> SAT in 2.039 s.
// [18:26:51] [D] Propagations: 8868239
// [18:26:51] [D] Conflicts: 1984
// [18:26:51] [D] Decisions: 99845
// [18:26:51] [I] BasicMin: minimal C = 8
// [18:26:52] [I] ExtendedTask: declared 19968 variables and 424128 clauses in 0.556 s.
// [18:27:28] [I] All done in 45.277 seconds
//
// Active-passive reduction with adhoc flags: (278bbaf9ac1d6527c135785124f3f508fdef7103)
// fbsat infer extended-min -P 5 -i data\tests-39.gz --glucose --debug --encode-active-passive --encode-epsilon-passive --encode-not-epsilon-active
// [18:29:44] [I] BasicTask: declared 107932 variables and 828311 clauses in 0.674 s.
// [18:29:45] [I] BasicMin: C = 8 -> SAT in 1.450 s.
// [18:29:45] [D] Propagations: 5415239
// [18:29:45] [D] Conflicts: 1132
// [18:29:45] [D] Decisions: 32339
// [18:29:45] [I] BasicMin: minimal C = 8
// [18:29:45] [I] ExtendedTask: declared 19968 variables and 424128 clauses in 0.332 s.
// [18:30:02] [I] All done in 24.202 seconds

fun Solver.declareActivePassivePositiveMappingConstraints(
    isEncodeReverseImplication: Boolean,
) {
    comment("Active-passive positive mapping constraints")

    val tree: ScenarioTree<*, *> = context["tree"]
    val V: Int = context["V"]
    val C: Int = context["C"]
    val K: Int = context["K"]
    val E: Int = context["E"]
    val Z: Int = context["Z"]
    val U: Int = context["U"]
    val stateOutputEvent: IntVarArray = context["stateOutputEvent"]
    val stateAlgorithmTop: BoolVarArray = context["stateAlgorithmTop"]
    val stateAlgorithmBot: BoolVarArray = context["stateAlgorithmBot"]
    val transitionFunction: IntVarArray = context["transitionFunction"]
    val actualTransitionFunction: IntVarArray = context["actualTransitionFunction"]
    val transitionDestination: IntVarArray = context["transitionDestination"]
    val mapping: IntVarArray = context["mapping"]
    val active: BoolVarArray = context["active"]

    comment("Transition function definition")
    // (atf[q,e,u] = q') => (tf[q,e,u] = q')
    for (i in 1..C)
        for (e in 1..E)
            for (u in 1..U)
                for (j in 1..C)
                    imply(
                        actualTransitionFunction[i, e, u] eq j,
                        transitionFunction[i, e, u] eq j
                    )
    // (atf[q,e,u] = 0) => (tf[q,e,u] = q)
    for (c in 1..C)
        for (e in 1..E)
            for (u in 1..U)
                imply(
                    actualTransitionFunction[c, e, u] eq 0,
                    transitionFunction[c, e, u] eq c
                )

    comment("Root maps to the first state")
    clause(mapping[1] eq 1)

    comment("Root is passive")
    clause(-active[1])

    comment("Mapping constraints")
    for (v in 2..V) {
        val p = tree.parent(v)
        val e = tree.inputEvent(v)
        val u = tree.inputNumber(v)
        val o = tree.outputEvent(v)

        // If any output value changed, then the node is definitely active
        if ((1..Z).any { z -> tree.outputValue(v, z) != tree.outputValue(p, z) }) {
            clause(active[v])
        }

        if (Globals.IS_ENCODE_EPSILON_PASSIVE) {
            // If output event is epsilon, then the node is definitely passive
            if (o == 0) {
                clause(-active[v])
            }
        }

        if (Globals.IS_ENCODE_NOT_EPSILON_ACTIVE) {
            // If output event is not epsilon, then the node is definitely active
            if (o != 0) {
                clause(active[v])
            }
        }

        comment("Positive mapping/transitionFunction definition for v = $v")
        // (mapping[p] = q) => ((mapping[v] = q') <=> (transitionFunction[q,e,u] = q'))
        for (i in 1..C)
            for (j in 1..C)
                implyIff(
                    mapping[p] eq i,
                    mapping[v] eq j,
                    transitionFunction[i, e, u] eq j
                )

        comment("Positive active-mapping definition for v = $v")
        // active[v] =>
        //  (mapping[v] = c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)] = c)
        for (i in 1..C)
            for (j in 1..C)
                implyImplyIff(
                    active[v],
                    mapping[p] eq i,
                    mapping[v] eq j,
                    actualTransitionFunction[i, e, u] eq j
                )
        // active[v] =>
        //  (mapping[v] = c) => (stateOutputEvent[c] = toe(v))
        for (c in 1..C)
            imply2(
                active[v],
                mapping[v] eq c,
                stateOutputEvent[c] eq o
            )
        // active[v] =>
        //  (mapping[v] = c) => AND_{z}(stateAlgorithm{tov(tp(v),z)}[c,z] = tov(v,z))
        for (c in 1..C)
            for (z in 1..Z)
                imply2(
                    active[v],
                    mapping[v] eq c,
                    algorithmChoice(
                        tree = tree,
                        v = v, c = c, z = z,
                        algorithmTop = stateAlgorithmTop,
                        algorithmBot = stateAlgorithmBot
                    )
                )

        comment("REVERSE actualTransitionFunction/active")
        // (mapping[p] = q) & (actualTransitionFunction[q,e,u] != 0) => active[v]
        for (c in 1..C)
            imply2(
                mapping[p] eq c,
                actualTransitionFunction[c, e, u] neq 0,
                active[v]
            )

        comment("Positive passive-mapping definition for v = $v")
        // ~active[v] =>
        //  mapping[v] = mapping[tp(v)]
        for (c in 1..C)
            imply2(
                -active[v],
                mapping[p] eq c,
                mapping[v] eq c
            )
        // ~active[v] =>
        //  (mapping[tp(v)] = c) => (actualTransition[c,tie(v),tin(v)] = 0)
        for (c in 1..C)
            imply2(
                -active[v],
                mapping[p] eq c,
                actualTransitionFunction[c, e, u] eq 0
            )

        comment("REVERSE actualTransitionFunction/passive")
        // (mapping[p] = q) & (actualTransitionFunction[q,e,u] = 0) => ~active[v]
        for (c in 1..C)
            imply2(
                mapping[p] eq c,
                actualTransitionFunction[c, e, u] eq 0,
                -active[v]
            )
    }

    if (isEncodeReverseImplication) {
        comment("Mysterious reverse-implication")
        // OR_k(transitionDestination[i,k,j]) => OR_{v}( mapping[p]=i & mapping[v]=j & active[v] )
        for (i in 1..C)
            for (j in 1..C) {
                val lhsAux = newLiteral()
                iffOr(lhsAux) {
                    for (k in 1..K)
                        yield(transitionDestination[i, k] eq j)
                }

                val rhsAux = newLiteral()
                iffOr(rhsAux) {
                    for (v in 2..V) {
                        val p = tree.parent(v)
                        val aux = newLiteral()
                        iffAnd(aux, mapping[p] eq i, mapping[v] eq j, active[v])
                        yield(aux)
                    }
                }

                imply(lhsAux, rhsAux)

                // Adhoc: other way around!
                imply(rhsAux, lhsAux)
            }
    }
}
