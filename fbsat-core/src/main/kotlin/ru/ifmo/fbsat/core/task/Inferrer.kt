package ru.ifmo.fbsat.core.task

import ru.ifmo.fbsat.core.solver.Solver
import java.io.File

class Inferrer(
    val solver: Solver,
    val outDir: File = File("out")
) {
    fun reset() {
        solver.reset()
    }

    fun declare(task: Task) {
        task.declare(solver)
    }
}
