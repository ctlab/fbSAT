package ru.ifmo.fbsat.core.task.distributed.extended

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.solver.DomainVarArray
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.solver.declareModularContext
import ru.ifmo.fbsat.core.solver.forEachModularContext
import ru.ifmo.fbsat.core.task.single.extended.declareExtendedVariables
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log

@Suppress("LocalVariableName")
fun Solver.declareDistributedExtendedVariables(
    modularP: MultiArray<Int>,
) {
    context["modularP"] = modularP

    /* Modular */
    forEachModularContext { m ->
        declareExtendedVariables(P = modularP[m])
    }

    /* Cardinality */
    val cardinalityN = context("cardinalityN") {
        declareCardinality {
            forEachModularContext {
                val C: Int = context["C"]
                val K: Int = context["K"]
                val P: Int = context["P"]
                val nodeType: DomainVarArray<NodeType> = context["nodeType"]
                for (c in 1..C)
                    for (k in 1..K)
                        for (p in 1..P)
                            yield(nodeType[c, k, p] neq NodeType.NONE)
            }
        }
    }

    if (Globals.IS_DUMP_VARS_IN_CNF) {
        log.warn("Dumping of DistributedExtendedVariables to CNF is not implemented yet")
    }
}
