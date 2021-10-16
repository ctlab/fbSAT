package ru.ifmo.fbsat.core.task

import com.github.lipen.satlib.solver.Solver
import com.soywiz.klock.measureTime
import ru.ifmo.fbsat.core.utils.MyLogger

private val logger = MyLogger {}

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
    val nVarsStart = numberOfVariables
    val nClausesStart = numberOfClauses

    val runningTime = measureTime { block() }

    val nVarsDiff = numberOfVariables - nVarsStart
    val nClausesDiff = numberOfClauses - nClausesStart
    logger.info(
        "$name: declared $nVarsDiff variables and $nClausesDiff clauses in %.3f s."
            .format(runningTime.seconds)
    )
}
