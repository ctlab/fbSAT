package ru.ifmo.fbsat.core.task.extended

import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareTotalizer

internal fun Solver.declareTotalizerExtended(baseReduction: BaseReduction): IntArray =
    declareTotalizer {
        for (c in 1..baseReduction.C)
            for (k in 1..baseReduction.K)
                for (p in 1..baseReduction.P)
                    yield(-baseReduction.nodeType[c, k, p, NodeType.NONE.value])
        // val (C, Z) = baseReduction.algorithm0.shape
        // for (c in 1..C)
        //     for (z in 1..Z) {
        //         yield(baseReduction.algorithm0[c, z])
        //         yield(-baseReduction.algorithm1[c, z])
        //     }
    }
