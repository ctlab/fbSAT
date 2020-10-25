package ru.ifmo.fbsat.core.task

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince

// FIXME: 'private constructor' is temporarily
abstract class Task private constructor(name: String? = null) {
    protected val name: String = name ?: this::class.java.simpleName

    // TODO: remove
    constructor() : this(null)

    @Suppress("FunctionName")
    protected abstract fun Solver.declare_()

    fun declare(solver: Solver) {
        solver.runWithLogging(name) { declare_() }
    }
}

@Deprecated("Use task.declare", ReplaceWith("task.declare(this)"))
fun Solver.declare(task: Task): Unit = task.declare(this)

private inline fun Solver.runWithLogging(name: String, block: Solver.() -> Unit) {
    val timeStart = PerformanceCounter.reference
    val nVarsStart = numberOfVariables
    val nClausesStart = numberOfClauses

    block()

    val nVarsDiff = numberOfVariables - nVarsStart
    val nClausesDiff = numberOfClauses - nClausesStart
    log.info(
        "$name: declared $nVarsDiff variables and $nClausesDiff clauses in %.3f s."
            .format(timeSince(timeStart).seconds)
    )
}
