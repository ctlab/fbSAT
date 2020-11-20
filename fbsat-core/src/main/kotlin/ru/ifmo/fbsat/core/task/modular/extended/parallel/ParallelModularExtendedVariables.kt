package ru.ifmo.fbsat.core.task.modular.extended.parallel

import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.modular.extended._declareModularExtendedVariables

@Suppress("LocalVariableName")
fun Solver.declareParallelModularExtendedVariables(
    P: Int,
) {
    _declareModularExtendedVariables(P)
}
