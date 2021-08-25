package ru.ifmo.fbsat.core.task.modular.extended

import com.github.lipen.satlib.card.declareCardinality
import com.github.lipen.satlib.core.DomainVarArray
import com.github.lipen.satlib.core.neq
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.automaton.guard.NodeType
import ru.ifmo.fbsat.core.solver.forEachModularContext
import ru.ifmo.fbsat.core.task.single.extended.declareExtendedVariables

@Suppress("LocalVariableName")
internal fun Solver._declareModularExtendedVariables(
    P: Int,
) {
    context["P"] = P

    /* Modular */
    forEachModularContext {
        declareExtendedVariables(P = P)
    }

    /* Cardinality */
    comment("Cardinality (N)")
    val cardinalityN = context("cardinalityN") {
        declareCardinality {
            @Suppress("NAME_SHADOWING")
            forEachModularContext {
                // val moduleCardinalityN: Cardinality = context["cardinalityN"]
                // yieldAll(moduleCardinalityN.totalizer.values)
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
}
