package ru.ifmo.fbsat.core.task.distributed.extended

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.solver.DomainVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.SolverContext
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.newContext
import ru.ifmo.fbsat.core.solver.switchContext
import ru.ifmo.fbsat.core.task.single.extended.declareExtendedVariables
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log

fun Solver.declareDistributedExtendedVariables(
    modularP: MultiArray<Int>
) {
    val M: Int by context

    /* Modular */
    val modularContext: MultiArray<SolverContext> by context(MultiArray.create(M) { newContext() })
    for (m in 1..M) switchContext(modularContext[m]) {
        declareExtendedVariables(P = modularP[m])
    }

    /* Cardinality */
    val cardinalityN by context(declareCardinality {
        for (m in 1..M) switchContext(modularContext[m]) {
            val C: Int by context
            val K: Int by context
            val P: Int by context
            val nodeType: DomainVarArray<NodeType> by context
            for (c in 1..C)
                for (k in 1..K)
                    for (p in 1..P)
                        yield(nodeType[c, k, p] neq NodeType.NONE)
        }
    })

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        log.warn("Dump of variables to CNF is not implemented yet")
    }
}
