package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.automaton.NodeType
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.solver.declareTotalizer

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
