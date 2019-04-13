package ru.ifmo.fbsat.core.task.basic

import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareTotalizer

internal fun Solver.declareTotalizer(baseReduction: BaseReduction): IntArray {
    return declareTotalizer(sequence {
        val (C, K, _) = baseReduction.transition.shape
        for (c in 1..C)
            for (k in 1..K)
                yield(-baseReduction.transition[c, k, C + 1])
    })
}
