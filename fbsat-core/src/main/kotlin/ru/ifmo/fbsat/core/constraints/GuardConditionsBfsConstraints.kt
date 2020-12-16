package ru.ifmo.fbsat.core.constraints

import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.op.imply
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.solver.forEachModularContext

@Suppress("LocalVariableName")
fun Solver.declareGuardConditionsBfsConstraints() {
    comment("Guard conditions BFS constraints")
    val C: Int = context["C"]
    val K: Int = context["K"]
    val P: Int = context["P"]
    val nodeParent: IntVarArray = context["nodeParent"]

    for (c in 1..C)
        for (k in 1..K) {
            comment("Guard conditions BFS constraints: for c = $c, k = $k")
            for (j in 3 until P)
                for (i in 2 until j)
                    for (s in 1 until i)
                        imply(
                            nodeParent[c, k, j] eq i,
                            nodeParent[c, k, j + 1] neq s
                        )
        }
}

fun Solver.declareParallelModularGuardConditionsBfsConstraints() {
    comment("Parallel modular guard conditions BFS constraints")
    forEachModularContext { m ->
        comment("Parallel modular guard conditions BFS constraints: for module m = $m")
        declareGuardConditionsBfsConstraints()
    }
}

fun Solver.declareConsecutiveModularGuardConditionsBfsConstraints() {
    comment("Consecutive modular guard conditions BFS constraints")
    forEachModularContext { m ->
        comment("Consecutive modular guard conditions BFS constraints: for module m = $m")
        declareGuardConditionsBfsConstraints()
    }
}

fun Solver.declareDistributedGuardConditionsBfsConstraints() {
    comment("Distributed guard conditions BFS constraints")
    forEachModularContext { m ->
        comment("Distributed guard conditions BFS constraints: for module m = $m")
        declareGuardConditionsBfsConstraints()
    }
}
