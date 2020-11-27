package ru.ifmo.fbsat.core.task

import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.utils.mylog
import java.io.File

class Inferrer(
    val solver: Solver,
    val outDir: File = File("out"),
) {
    // Note: must use `get()` because `solver.context` is `var`.
    // val context: SolverContext get() = solver.context

    init {
        mylog.info("Inferrer created with solver = $solver, outDir = $outDir")
    }

    fun reset() {
        solver.reset()
    }

    fun declare(task: Task) {
        task.declare(solver)
    }
}
