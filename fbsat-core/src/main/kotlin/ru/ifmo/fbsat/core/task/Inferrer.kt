package ru.ifmo.fbsat.core.task

import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.utils.MyLogger
import java.io.File

private val logger = MyLogger {}

class Inferrer(
    val solver: Solver,
    val outDir: File = File("out"),
) {
    // Note: must use `get()` because `solver.context` is `var`.
    // val context: SolverContext get() = solver.context

    init {
        logger.debug { "Inferrer created with solver = $solver, outDir = '$outDir'" }
    }

    fun reset() {
        solver.reset()
    }

    fun declare(task: Task) {
        task.declare(solver)
    }
}
