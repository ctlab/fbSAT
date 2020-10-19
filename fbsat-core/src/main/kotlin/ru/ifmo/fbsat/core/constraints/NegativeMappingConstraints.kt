package ru.ifmo.fbsat.core.constraints

import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.solver.BoolVarArray
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.SolverContext
import ru.ifmo.fbsat.core.solver.imply
import ru.ifmo.fbsat.core.solver.implyIffAnd
import ru.ifmo.fbsat.core.solver.implyImply
import ru.ifmo.fbsat.core.solver.implyOr
import ru.ifmo.fbsat.core.utils.algorithmChoice

fun Solver.declareNegativeMappingConstraints(
    context: SolverContext = this.context,
    Vs: Iterable<Int>,
    isForbidLoops: Boolean = true
) {
    comment("Negative mapping constraints")
    val negativeScenarioTree: NegativeScenarioTree by context

    /* Constraints for the root */
    // Skip constraints for the root if they are already defined
    if (1 in Vs) {
        comment("Negative mapping constraints: for root")
        declareNegativeMappingConstraintsForRoot(context = context)
    }

    /* Constraints for active vertices */
    comment("Negative mapping constraints: for active nodes")
    for (v in Vs.intersect(negativeScenarioTree.activeVertices)) {
        comment("Negative mapping constraints: for active node v = $v")
        declareNegativeMappingConstraintsForActiveNode(context = context, v = v)
    }

    /* Constraints for passive vertices */
    comment("Negative mapping constraints: for passive nodes")
    for (v in Vs.intersect(negativeScenarioTree.passiveVertices)) {
        comment("Negative mapping constraints: for passive node v = $v")
        declareNegativeMappingConstraintsForPassiveNode(context = context, v = v)
    }

    /* Additional constraints */
    comment("Additional negative mapping constraints")
    val negMapping: IntVarArray by context

    for (v in Vs.filter { it != 1 }) {
        val p = negativeScenarioTree.parent(v)

        comment("Non-satisfaction propagation: for node v = $v")
        // (negMapping[tp(v)]=0) => (negMapping[v]=0)
        imply(
            negMapping[p] eq 0,
            negMapping[v] eq 0
        )
    }

    if (isForbidLoops) {
        val C: Int by context
        val negV: Int by context
        val forbiddenLoops: MutableSet<Pair<Int, Int>> by context

        comment("Forbid loops")
        // (negMapping[v]=c) => AND_{l in loopBacks(v)}(negMapping[l] != c)
        for (v in 1..negV)
            for (l in negativeScenarioTree.loopBacks(v))
                if (forbiddenLoops.add(v to l))
                    for (c in 1..C)
                        imply(
                            negMapping[v] eq c,
                            negMapping[l] neq c
                        )
    }
}

private fun Solver.declareNegativeMappingConstraintsForRoot(
    context: SolverContext
) {
    val negMapping: IntVarArray by context

    comment("Root maps to the initial state")
    clause(negMapping[1] eq 1)
}

private fun Solver.declareNegativeMappingConstraintsForActiveNode(
    context: SolverContext,
    v: Int
) {
    val tree: NegativeScenarioTree = context["negativeScenarioTree"]
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)
    val o = tree.outputEvent(v)

    if (e == 0) {
        // log.warn("Empty input event when declaring mapping constraints for the active node v = $v!")
        // return
        error("This is unexpected")
    }

    val C: Int by context
    val Z: Int by context
    val stateOutputEvent: IntVarArray by context
    val stateAlgorithmTop: BoolVarArray by context
    val stateAlgorithmBot: BoolVarArray by context
    val negActualTransitionFunction: IntVarArray by context
    val negMapping: IntVarArray by context

    comment("Negative mapping definition for active node v = $v")
    // (mapping[v]=c) <=> (actualTransition[mapping[tp(v)],tie(v),tin(v)]=c) & (stateOutputEvent[c]=toe(v)) & AND_{z}(stateAlgorithm{tov(tp(v),z)}(c,z) = tov(v,z))
    for (i in 1..C)
        for (j in 1..C)
            implyIffAnd(
                negMapping[p] eq i,
                negMapping[v] eq j,
                sequence {
                    yield(negActualTransitionFunction[i, e, u] eq j)
                    yield(stateOutputEvent[j] eq o)
                    for (z in 1..Z)
                        yield(
                            algorithmChoice(
                                tree = tree,
                                v = v, c = j, z = z,
                                algorithmTop = stateAlgorithmTop,
                                algorithmBot = stateAlgorithmBot
                            )
                        )
                }
            )
}

private fun Solver.declareNegativeMappingConstraintsForPassiveNode(
    context: SolverContext,
    v: Int
) {
    val tree: NegativeScenarioTree = context["negativeScenarioTree"]
    val p = tree.parent(v)
    val e = tree.inputEvent(v)
    val u = tree.inputNumber(v)

    val C: Int by context
    val negActualTransitionFunction: IntVarArray by context
    val negMapping: IntVarArray by context

    if (e == 0) {
        // log.warn("Empty input event when declaring mapping constraints for the passive node v = $v!")

        for (c in 1..C) {
            imply(
                negMapping[p] eq c,
                negMapping[v] eq c
            )
        }

        return
    }

    comment("Mapping (negative) propagation for passive node v = $v")
    // (negMapping[v] = negMapping[tp(v)]) | (negMapping[v] = 0)
    for (c in 1..C)
        implyOr(
            negMapping[p] eq c,
            negMapping[v] eq c,
            negMapping[v] eq 0
        )

    comment("Constraining negActualTransitionFunction for passive node v = $v")
    // (negMapping[v] = c) => (negActualTransition[c,tie(v),tin(v)] = 0)
    for (c in 1..C)
        imply(
            negMapping[v] eq c,
            negActualTransitionFunction[c, e, u] eq 0
        )
    // (negMapping[v] = 0) => (negActualTransition[mapping[tp(v)],tie(v),tin(v)] != 0)
    for (c in 1..C)
        implyImply(
            negMapping[p] eq c,
            negMapping[v] eq 0,
            negActualTransitionFunction[c, e, u] neq 0
        )
}
