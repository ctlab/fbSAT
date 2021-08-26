package ru.ifmo.fbsat.core.constraints.new

import com.github.lipen.satlib.core.eq
import com.github.lipen.satlib.core.newBoolVarArray
import com.github.lipen.satlib.core.newDomainVarArray
import com.github.lipen.satlib.core.newIntVarArray
import com.github.lipen.satlib.core.sign
import com.github.lipen.satlib.op.iff
import com.github.lipen.satlib.op.iffAnd
import com.github.lipen.satlib.op.iffOr
import com.github.lipen.satlib.op.implyAnd
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.scenario.ScenarioTree
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.imply2
import ru.ifmo.fbsat.core.solver.imply2And

enum class StateAction {
    Same, Zero, One
}

fun Solver.declareNewConstraints() {
    comment("New constraints")

    val tree: ScenarioTree<*, *> = context["tree"]
    val V: Int = context["V"]
    val C: Int = context["C"]
    val K: Int = context["K"]
    val X: Int = context["X"]
    val Z: Int = context["Z"]
    val U: Int = context["U"]
    val stateOutputAction = newDomainVarArray(C, Z) { StateAction.values().asIterable() }
    // For `stateOutputFunction`: 1 is true, 2 is false
    val stateOutputFunction = newBoolVarArray(C, Z, 2)
    val transitionDestination = newIntVarArray(C, K) { 0..C }
    val transitionTruthTable = newBoolVarArray(C, K, U)
    val transitionFiring = newBoolVarArray(C, K, U)
    val firstFired = newIntVarArray(C, U) { 0..K }
    val notFired = newBoolVarArray(C, K, U)
    val actualTransitionFunction = newIntVarArray(C, U) { 0..C } // activeNextState
    val mapping = newIntVarArray(V) { 1..C }
    val active = newBoolVarArray(V)
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

    comment("State action semantics")
    for (c in 1..C)
        for (z in 1..Z) {
            // Note: 1 is true, 2 is false

            // (stateOutputAction[c,z] = Same) => stateOutputFunction[c,z,1]
            // (stateOutputAction[c,z] = Same) => ~stateOutputFunction[c,z,2]
            implyAnd(
                stateOutputAction[c, z] eq StateAction.Same,
                stateOutputFunction[c, z, 1],
                -stateOutputFunction[c, z, 2]
            )

            // (stateOutputAction[c,z] = Zero) => ~stateOutputFunction[c,z,1]
            // (stateOutputAction[c,z] = Zero) => ~stateOutputFunction[c,z,2]
            implyAnd(
                stateOutputAction[c, z] eq StateAction.Zero,
                -stateOutputFunction[c, z, 1],
                -stateOutputFunction[c, z, 2]
            )

            // (stateOutputAction[c,z] = One) => stateOutputFunction[c,z,1]
            // (stateOutputAction[c,z] = One) => stateOutputFunction[c,z,2]
            implyAnd(
                stateOutputAction[c, z] eq StateAction.One,
                stateOutputFunction[c, z, 1],
                stateOutputFunction[c, z, 2]
            )

            // TODO: flip
            // (stateOutputAction[c,z] = Flip) => ~stateOutputFunction[c,z,1]
            // (stateOutputAction[c,z] = Flip) => stateOutputFunction[c,z,2]
            // implyAnd(
            //     stateOutputAction[c, z] eq StateAction.Flip,
            //     stateOutputFunction[c, z, 1],
            //     stateOutputFunction[c, z, 2]
            // )
        }

    comment("Transition semantics")

    // for (u in 1..U)
    //     for (i in 1..C)
    //         for (k in 1..K)
    //             for (j in 1..C)
    //                 imply2(
    //                     firstFired[i, u] eq k,
    //                     transitionDestination[i, k] eq j,
    //                     actualTransitionFunction[i, u] eq j
    //                 )

    // (actualTransitionFunction[q,u] = q') <=>
    //   OR_k( (transitionDestination[q,k] = q') & (firstFired[q,u] = k) )
    for (i in 1..C)
        for (u in 1..U)
            for (j in 1..C)
                iffOr(actualTransitionFunction[i, u] eq j) {
                    for (k in 1..K) {
                        // aux <=> (transitionDestination[q,k] = q') & (firstFired[q,u] = k)
                        val aux = newLiteral()
                        iffAnd(
                            aux,
                            transitionDestination[i, k] eq j,
                            firstFired[i, u] eq k
                        )
                        yield(aux)
                    }
                }

    // TODO: this constraint can be implemented in the variable declaration (reuse literals)
    // (actualTransitionFunction[q,u] = 0) <=> (firstFired[q,u] = 0)
    for (c in 1..C)
        for (u in 1..U)
            iff(
                actualTransitionFunction[c, u] eq 0,
                firstFired[c, u] eq 0
            )

    comment("Mapping semantics")
    for (v in 1..V) {
        val p = tree.parent(v)
        val u = tree.inputNumber(v)

        // If any output value changed, then the node is definitely active
        if ((1..Z).any { z -> tree.outputValue(v, z) != tree.outputValue(p, v) }) {
            clause(active[v])
        }

        // TODO: activeMapping
        // ((mapping[p] = q) & (actualTransitionFunction[q,u] = q')) => (mapping[v] = q') & active[v]
        for (i in 1..C)
            for (j in 1..C)
                imply2And(
                    mapping[p] eq i,
                    actualTransitionFunction[i, u] eq j,
                    mapping[v] eq j,
                    active[v]
                )

        // TODO: passiveMapping
        // ((mapping[p] = q) & (actualTransitionFunction[q,u] = 0)) => (mapping[v] = q) & ~active[v]
        for (c in 1..C)
            imply2And(
                mapping[p] eq c,
                actualTransitionFunction[c, u] eq 0,
                mapping[v] eq c,
                -active[v]
            )

        // TODO: activeMapping
        // ((mapping[v] = q) & active[v]) => (stateOutputFunction[q,z,tov(p,z)] = tov(v,z))
        for (c in 1..C)
            for (z in 1..Z)
                imply2(
                    mapping[v] eq c,
                    active[v],
                    stateOutputFunction[c, z, if (tree.outputValue(p, z)) 1 else 2] sign tree.outputValue(v, z)
                )
    }

    // comment("Mapping")
    // for (v in 1..V) {
    //     val p = tree.parent(v)
    //     val u = tree.inputNumber(v)
    //
    //     // (mapping[v] = c) => (nextState[mapping[tp(v)],tin(v)] = c)
    //     for (i in 1..C)
    //         for (j in 1..C)
    //             implyImply(
    //                 mapping[p] eq i,
    //                 mapping[v] eq j,
    //                 nextState[i, u] eq j
    //             )
    //
    //     // (mapping[v] = c) => (stateOutputFunction[c,z,tov(p,z)] = tov(v,z))
    //     for (c in 1..C)
    //         for (z in 1..Z)
    //             imply(
    //                 mapping[v] eq c,
    //                 stateOutputFunction[c, z, if (tree.outputValue(p, z)) 1 else 2] sign tree.outputValue(v, z)
    //             )
    // }
}
