package ru.ifmo.fbsat.core.task

import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.utils.log
import java.io.File

class Inferrer(
    val solver: Solver,
    val outDir: File = File("out")
) {
    init {
        log.info("Inferrer created with solver = $solver, outDir = $outDir")
    }

    fun reset() {
        solver.reset()
    }

    fun declare(task: Task) {
        task.declare(solver)
    }
}
