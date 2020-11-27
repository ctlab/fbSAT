package ru.ifmo.fbsat.core.task.modular.extended.parallel

import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.task.modular.extended._declareModularExtendedVariables

@Suppress("LocalVariableName")
fun Solver.declareParallelModularExtendedVariables(
    P: Int,
) {
    _declareModularExtendedVariables(P)
}
