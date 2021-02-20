package ru.ifmo.fbsat.core.task

import com.github.lipen.satlib.core.LitArray
import com.github.lipen.satlib.core.Model
import com.github.lipen.satlib.solver.Solver
import com.soywiz.klock.PerformanceCounter
import com.soywiz.klock.TimeSpan
import ru.ifmo.fbsat.core.solver.runWithTimeout
import ru.ifmo.fbsat.core.solver.solveAndGetModel
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.timeSince
import java.io.File

private val logger = MyLogger {}

class Inferrer(
    val solver: Solver,
    val outDir: File = File("out"),
    val timeout: Long? = null, // milliseconds
    val timeStart: TimeSpan = PerformanceCounter.reference,
) {
    // TODO: inline the function call
    private val timeoutLeft: Long?
        get() = timeoutLeft(timeout, timeStart)

    // Note: must use `get()` because `solver.context` is `var`.
    // val context: SolverContext get() = solver.context

    init {
        logger.debug { "Inferrer created with solver = $solver, outDir = '$outDir', timeout = $timeout" }
    }

    fun reset() {
        solver.reset()
    }

    fun declare(task: Task) {
        task.declare(solver)
    }

    fun isTimeout(): Boolean {
        val left = timeoutLeft
        return when {
            left == null -> false
            left <= 0 -> true
            else -> false
        }
    }

    fun solve(): Boolean =
        solver.runWithTimeout(timeoutLeft) {
            solve()
        }

    fun solve(assumptions: Iterable<Int>): Boolean =
        solver.runWithTimeout(timeoutLeft) {
            solve(assumptions)
        }

    fun solve(assumptions: LitArray): Boolean =
        solver.runWithTimeout(timeoutLeft) {
            solve(assumptions)
        }

    fun solveAndGetModel(): Model? =
        solver.runWithTimeout(timeoutLeft) {
            solveAndGetModel()
        }
}

private fun timeoutLeft(timeout: Long?, timeStart: TimeSpan): Long? =
    timeout?.let { it - timeSince(timeStart).millisecondsLong }
