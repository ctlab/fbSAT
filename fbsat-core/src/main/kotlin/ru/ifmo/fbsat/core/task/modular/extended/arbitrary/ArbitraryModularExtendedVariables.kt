package ru.ifmo.fbsat.core.task.modular.extended.arbitrary

import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.task.modular.extended._declareModularExtendedVariables

@Suppress("LocalVariableName")
fun Solver.declareArbitraryModularExtendedVariables(
    P: Int,
) {
    _declareModularExtendedVariables(P)
}
