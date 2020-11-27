package ru.ifmo.fbsat.core.task.modular.extended.consecutive

import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.task.modular.extended._declareModularExtendedVariables

@Suppress("LocalVariableName")
fun Solver.declareConsecutiveModularExtendedVariables(
    P: Int,
) {
    _declareModularExtendedVariables(P)
}
