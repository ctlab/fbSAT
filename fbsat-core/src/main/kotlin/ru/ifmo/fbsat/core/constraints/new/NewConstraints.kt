package ru.ifmo.fbsat.core.constraints.new

import com.github.lipen.satlib.core.BoolVarArray
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.eq
import com.github.lipen.satlib.core.neq
import com.github.lipen.satlib.core.sign
import com.github.lipen.satlib.op.iffAnd
import com.github.lipen.satlib.op.iffOr
import com.github.lipen.satlib.op.imply
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.constraints.declareAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareAutomatonStructureConstraintsForInputs
import ru.ifmo.fbsat.core.constraints.declareAutomatonStructureConstraintsInputless
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.imply2
import ru.ifmo.fbsat.core.solver.imply3
import ru.ifmo.fbsat.core.utils.Globals

// Old reduction:
// [23:54:36] [I] BasicTask: declared 63900 variables and 305729 clauses in 0.363 s.
// [23:54:36] [I] BasicMin: C = 8 -> SAT in 0.604 s.
// [23:54:36] [D] Propagations: 2071667
// [23:54:36] [D] Conflicts: 411
// [23:54:36] [D] Decisions: 37311
// [23:54:36] [I] BasicMin: minimal C = 8
// [23:54:36] [I] ExtendedTask: declared 19968 variables and 424128 clauses in 0.354 s.
// [23:54:44] [I] All done in 12.516 seconds
//
// New reduction:
// [23:57:22] [I] BasicTask: declared 107932 variables and 766935 clauses in 0.608 s.
// [23:57:22] [I] BasicMin: C = 8 -> SAT in 1.131 s.
// [23:57:22] [D] Propagations: 3122233
// [23:57:22] [D] Conflicts: 705
// [23:57:22] [D] Decisions: 17120
// [23:57:22] [I] BasicMin: minimal C = 8
// [23:57:23] [I] ExtendedTask: declared 19968 variables and 424128 clauses in 0.369 s.
// [23:57:42] [I] All done in 25.830 seconds

enum class StateAction {
    Same, Zero, One
}

fun Solver.declareNewConstraints() {
    comment("New constraints")

    val tree: ScenarioTree<*, *> = context["tree"]
    val V: Int = context["V"]
    val C: Int = context["C"]
    val K: Int = context["K"]
    val E: Int = context["E"]
    val X: Int = context["X"]
    val Z: Int = context["Z"]
    val U: Int = context["U"]
    // val stateOutputAction: DomainVarArray<StateAction> = context["stateOutputAction"]
    val stateOutputEvent: IntVarArray = context["stateOutputEvent"]
    val stateAlgorithmTop: BoolVarArray = context["stateAlgorithmTop"]
    val stateAlgorithmBot: BoolVarArray = context["stateAlgorithmBot"]
    val transitionFunction: IntVarArray = context["transitionFunction"]
    val actualTransitionFunction: IntVarArray = context["actualTransitionFunction"]
    val transitionDestination: IntVarArray = context["transitionDestination"]
    val transitionInputEvent: IntVarArray = context["transitionInputEvent"]
    val transitionTruthTable: BoolVarArray = context["transitionTruthTable"]
    val transitionFiring: BoolVarArray = context["transitionFiring"]
    val firstFired: IntVarArray = context["firstFired"]
    val notFired: BoolVarArray = context["notFired"]
    val mapping: IntVarArray = context["mapping"]
    val active: BoolVarArray = context["active"]
    // val activeMapping: IntVarArray = context["activeMapping"]
    // TODO: active/passive mapping:
    //   activeMapping = newIntVarArray(V) { 0..C }
    //   // Note: this 0 is NOT the same as in the negative tree.
    //   //       What happens in the negative tree is a big open question...
    //   (activeMapping[v] = q) <=> (mapping[v] = q) & active[v]
    //   active[v] => (activeMapping[v] != 0)
    //   ...same for passive:
    //   (passiveMapping[v] = q) <=> (mapping[v] = q) & ~active[v]
    //   ...interaction:
    //   (activeMapping[v] != 0) <=> (passiveMapping[v] = 0)

    declareAutomatonStructureConstraintsInputless()
    declareAutomatonStructureConstraintsForInputs(1..U, isPositive = true)
    if (Globals.IS_BFS_AUTOMATON) declareAutomatonBfsConstraints()

    // comment("State action semantics")
    // for (c in 1..C)
    //     for (z in 1..Z) {
    //         // Note: 1 is true, 2 is false
    //
    //         // (stateOutputAction[c,z] = Same) => stateOutputFunction[c,z,1]
    //         // (stateOutputAction[c,z] = Same) => ~stateOutputFunction[c,z,2]
    //         implyAnd(
    //             stateOutputAction[c, z] eq StateAction.Same,
    //             stateAlgorithmTop[c, z],
    //             -stateAlgorithmBot[c, z]
    //         )
    //
    //         // (stateOutputAction[c,z] = Zero) => ~stateOutputFunction[c,z,1]
    //         // (stateOutputAction[c,z] = Zero) => ~stateOutputFunction[c,z,2]
    //         implyAnd(
    //             stateOutputAction[c, z] eq StateAction.Zero,
    //             -stateAlgorithmTop[c, z],
    //             -stateAlgorithmBot[c, z]
    //         )
    //
    //         // (stateOutputAction[c,z] = One) => stateOutputFunction[c,z,1]
    //         // (stateOutputAction[c,z] = One) => stateOutputFunction[c,z,2]
    //         implyAnd(
    //             stateOutputAction[c, z] eq StateAction.One,
    //             stateAlgorithmTop[c, z],
    //             stateAlgorithmBot[c, z]
    //         )
    //
    //         // TODO: flip
    //         // (stateOutputAction[c,z] = Flip) => ~stateOutputFunction[c,z,1]
    //         // (stateOutputAction[c,z] = Flip) => stateOutputFunction[c,z,2]
    //         // implyAnd(
    //         //     stateOutputAction[c, z] eq StateAction.Flip,
    //         //     stateAlgorithmTop[c,z],
    //         //     stateAlgorithmBot[c,z]
    //         // )
    //     }

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

        // If output event is epsilon, then the node is definitely passive
        if (o == 0) {
            clause(-active[v])
        }

        // If output event is not epsilon, then the node is definitely active
        if (o != 0) {
            clause(active[v])
        }

        comment("transitionFunction definition")
        // (mapping[p] = q) & (mapping[v] = q') => (transitionFunction[q,e,u] = q')
        for (i in 1..C)
            for (j in 1..C)
                imply2(
                    mapping[p] eq i,
                    mapping[v] eq j,
                    transitionFunction[i, e, u] eq j
                )

        comment("actualTransitionFunction definition")
        // (mapping[p] = q) & (mapping[v] = q') & active[v] => (actualTransitionFunction[q,e,u] = q')
        for (i in 1..C)
            for (j in 1..C)
                imply3(
                    mapping[p] eq i,
                    mapping[v] eq j,
                    active[v],
                    actualTransitionFunction[i, e, u] eq j
                )
        // (mapping[p] = q) & ~active[v] => (actualTransitionFunction[q,e,u] = 0)
        for (c in 1..C)
            imply2(
                mapping[p] eq c,
                -active[v],
                actualTransitionFunction[c, e, u] eq 0
            )

        comment("Passive mapping propagation")
        // (mapping[p] = q) & ~active[v] => (mapping[v] = q)
        for (c in 1..C)
            imply2(
                mapping[p] eq c,
                -active[v],
                mapping[v] eq c
            )

        comment("Active mapping semantics: output event")
        // (mapping[v] = q) & active[v] => (stateOutputEvent[q] = toe(v))
        for (c in 1..C)
            imply2(
                mapping[v] eq c,
                active[v],
                stateOutputEvent[c] eq o
            )

        comment("Active mapping semantics: output values")
        // (mapping[v] = q) & active[v] => (stateOutputFunction[q,z,tov(p,z)] = tov(v,z))
        for (c in 1..C)
            for (z in 1..Z)
                imply2(
                    mapping[v] eq c,
                    active[v],
                    if (tree.outputValue(p, z)) {
                        stateAlgorithmTop[c, z]
                    } else {
                        stateAlgorithmBot[c, z]
                    } sign tree.outputValue(v, z)
                )

        comment("REVERSE transitionFunction/mapping")
        // (mapping[p] = q) & (transitionFunction[q,e,u] = q') => (mapping[v] = q')
        for (i in 1..C)
            for (j in 1..C)
                imply2(
                    mapping[p] eq i,
                    transitionFunction[i, e, u] eq j,
                    mapping[v] eq j
                )

        comment("REVERSE actualTransitionFunction/active")
        // (mapping[p] = q) & (actualTransitionFunction[q,e,u] != 0) => active[v]
        for (c in 1..C)
            imply2(
                mapping[p] eq c,
                actualTransitionFunction[c, e, u] neq 0,
                active[v]
            )
        // (mapping[p] = q) & (actualTransitionFunction[q,e,u] = 0) => ~active[v]
        for (c in 1..C)
            imply2(
                mapping[p] eq c,
                actualTransitionFunction[c, e, u] eq 0,
                -active[v]
            )
    }

    if (Globals.IS_ENCODE_REVERSE_IMPLICATION) {
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
                // imply(rhsAux, lhsAux)
            }
    }
}
