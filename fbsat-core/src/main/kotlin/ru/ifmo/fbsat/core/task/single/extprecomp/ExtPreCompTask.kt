package ru.ifmo.fbsat.core.task.single.extprecomp

import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.task.Task

data class ExtPreCompTask(
    val maxGuardSize: Int, // P
    val maxTotalGuardsSize: Int? = null, // N, unconstrained if null
) : Task() {
    override fun Solver.declare_() {
        TODO()
    }
}
