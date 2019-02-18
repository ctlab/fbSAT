package ru.ifmo.fbsat.task.extended

import ru.ifmo.fbsat.automaton.NodeType
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.solver.declareTotalizer

fun Solver.declareTotalizerExtended(baseReduction: BaseReduction): IntArray =
    declareTotalizer {
        for (c in 1..baseReduction.C)
            for (k in 1..baseReduction.K)
                for (p in 1..baseReduction.P)
                    yield(-baseReduction.nodeType[c, k, p, NodeType.NONE.value])
    }
