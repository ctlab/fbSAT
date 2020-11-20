package ru.ifmo.fbsat.core.task.modular.extended.consecutive

import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.modular.extended._declareModularExtendedVariables

@Suppress("LocalVariableName")
fun Solver.declareConsecutiveModularExtendedVariables(
    P: Int,
) {
    _declareModularExtendedVariables(P)
}
