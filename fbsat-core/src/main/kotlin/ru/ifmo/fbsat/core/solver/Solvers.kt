package ru.ifmo.fbsat.core.solver

import com.soywiz.klock.DateTime
import okio.Buffer
import okio.buffer
import okio.sink
import okio.source
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.secondsSince
import ru.ifmo.fbsat.core.utils.timeIt

// TODO: make solver able to reset

interface Solver {
    val numberOfVariables: Int
    val numberOfClauses: Int

    fun newLiteral(): Literal

    fun <T> newVar(
        domain: Iterable<T>,
        encoding: VarEncoding = Globals.defaultVarEncoding,
        init: (T) -> Literal = { newLiteral() }
    ): Var<T>

    fun newIntVar(
        domain: Iterable<Int>,
        encoding: VarEncoding = Globals.defaultVarEncoding,
        init: (Int) -> Literal = { newLiteral() }
    ): IntVar

    fun newBoolVarArray(
        vararg shape: Int,
        init: (IntArray) -> Literal = { newLiteral() }
    ): BoolVarArray

    fun <T> Solver.newVarArray(
        vararg shape: Int,
        encoding: VarEncoding = Globals.defaultVarEncoding,
        domain: (IntArray) -> Iterable<T>
    ): VarArray<T>

    fun Solver.newIntVarArray(
        vararg shape: Int,
        encoding: VarEncoding = Globals.defaultVarEncoding,
        domain: (IntArray) -> Iterable<Int>
    ): IntVarArray

    fun comment(comment: String)

    fun clause(literals: Iterable<Literal>)
    fun clause(vararg literals: Literal)
    fun clause(literals: Sequence<Literal>)
    fun clause(block: suspend SequenceScope<Literal>.() -> Unit)

    fun solve(): RawAssignment?
    fun finalize2()

    companion object {
        const val trueVariable: Literal = Int.MAX_VALUE
        const val falseVariable: Literal = -trueVariable

        @JvmStatic
        fun default(command: String): Solver = DefaultSolver(command)

        @JvmStatic
        fun incremental(command: String): Solver = IncrementalSolver(command)

        fun mock(
            comment: (String) -> Unit = {},
            _clause: (List<Literal>) -> Unit = {},
            _solve: () -> BooleanArray? = { TODO() },
            finalize2: () -> Unit = {}
        ): Solver = object : AbstractSolver() {
            override fun comment(comment: String) = comment(comment)
            override fun _clause(literals: List<Literal>) = _clause(literals)
            override fun _solve(): BooleanArray? = _solve()
            override fun finalize2() = finalize2()
        }
    }
}

private abstract class AbstractSolver : Solver {
    final override var numberOfVariables = 0
        protected set
    final override var numberOfClauses = 0
        protected set

    final override fun newLiteral(): Literal = ++numberOfVariables

    final override fun <T> newVar(
        domain: Iterable<T>,
        encoding: VarEncoding,
        init: (T) -> Literal
    ): Var<T> = Var.create(domain, this, encoding, init)

    final override fun newIntVar(
        domain: Iterable<Int>,
        encoding: VarEncoding,
        init: (Int) -> Literal
    ): IntVar = IntVar.create(domain, this, encoding, init)

    final override fun newBoolVarArray(
        vararg shape: Int,
        init: (IntArray) -> Literal
    ): BoolVarArray = BoolVarArray.create(shape, init)

    final override fun <T> Solver.newVarArray(
        vararg shape: Int,
        encoding: VarEncoding,
        domain: (IntArray) -> Iterable<T>
    ): VarArray<T> = VarArray.create(shape) { index -> newVar(domain(index), encoding) }

    final override fun Solver.newIntVarArray(
        vararg shape: Int,
        encoding: VarEncoding,
        domain: (IntArray) -> Iterable<Int>
    ): IntVarArray = IntVarArray.create(shape) { index -> newIntVar(domain(index), encoding) }

    @Suppress("FunctionName")
    protected abstract fun _clause(literals: List<Literal>)

    final override fun clause(literals: Iterable<Literal>) {
        val pool = literals.filter { it != Solver.falseVariable }
        if (Solver.trueVariable in pool) return
        if (pool.isNotEmpty()) {
            ++numberOfClauses
            _clause(pool)
        }
    }

    final override fun clause(vararg literals: Literal) =
        clause(literals.asIterable())

    final override fun clause(literals: Sequence<Literal>) =
        clause(literals.asIterable())

    final override fun clause(block: suspend SequenceScope<Literal>.() -> Unit) =
        clause(sequence(block).constrainOnce())

    @Suppress("FunctionName")
    protected abstract fun _solve(): BooleanArray?

    final override fun solve(): RawAssignment? = _solve()?.let { RawAssignment(it) }
}

private class DefaultSolver(private val command: String) : AbstractSolver() {
    private val buffer = Buffer()

    override fun comment(comment: String) {
        log.debug { "// $comment" }
        buffer.writeUtf8("c ").writeUtf8(comment).writeUtf8("\n")
    }

    override fun _clause(literals: List<Literal>) {
        for (x in literals)
            buffer.writeUtf8(x.toString()).writeUtf8(" ")
        buffer.writeUtf8("0\n")
    }

    override fun _solve(): BooleanArray? {
        val process = Runtime.getRuntime().exec(command)
        val processInput = process.outputStream.sink().buffer()
        processInput.writeUtf8("p cnf $numberOfVariables $numberOfClauses\n")
        buffer.copyTo(processInput.buffer)

        log.debug { "Solving..." }
        val timeSolveStart = DateTime.now()
        processInput.close()

        var isSat: Boolean? = null
        val rawAssignment: MutableList<Boolean> = mutableListOf()

        process.inputStream.bufferedReader().useLines { lines ->
            label@ for (line in lines.map(String::trim)) {
                when {
                    line == "s SATISFIABLE" -> {
                        val timeSolve = secondsSince(timeSolveStart)
                        log.success("SAT in %.2f seconds".format(timeSolve))
                        isSat = true
                    }
                    line == "s UNSATISFIABLE" -> {
                        val timeSolve = secondsSince(timeSolveStart)
                        log.failure("UNSAT in %.2f seconds".format(timeSolve))
                        isSat = false
                        continue@label
                    }
                    line.startsWith("v ") -> {
                        rawAssignment.addAll(
                            line.splitToSequence(" ")
                                .drop(1) // drop "v"
                                .map { it.toInt() }
                                .takeWhile { it != 0 }
                                .map { it > 0 }
                        )
                    }
                }
            }
        }

        process.destroy()

        return when (isSat) {
            true -> rawAssignment.toBooleanArray()
            false -> null
            null -> error("Implicit UNSAT or ERROR")
        }
    }

    override fun finalize2() {}
}

private class IncrementalSolver(command: String) : AbstractSolver() {
    private val process = Runtime.getRuntime().exec(command)
    private val processInput = process.outputStream.sink().buffer()
    private val processOutput = process.inputStream.source().buffer()
    private val buffer = Buffer()

    override fun comment(comment: String) {
        log.debug { "// $comment" }
        processInput.writeUtf8("c ").writeUtf8(comment).writeUtf8("\n")
        buffer.writeUtf8("c ").writeUtf8(comment).writeUtf8("\n")
    }

    override fun _clause(literals: List<Literal>) {
        for (x in literals)
            processInput.writeUtf8(x.toString()).writeUtf8(" ")
        processInput.writeUtf8("0\n")

        for (x in literals)
            buffer.writeUtf8(x.toString()).writeUtf8(" ")
        buffer.writeUtf8("0\n")
    }

    override fun _solve(): BooleanArray? {
        processInput.writeUtf8("solve 0\n")
        processInput.flush()

        log.debug { "Solving..." }
        val (answer, solvingTime) = timeIt { processOutput.readUtf8Line() }
        log.debug { "Done solving in %.2f s.".format(solvingTime) }

        if (answer == null) {
            // log.error("Solver returned nothing")
            return null
        }

        when (answer) {
            "SAT" -> {
                // log.success("SAT in %.2f s".format(timeSolve))
                val line = processOutput.readUtf8Line() ?: return null
                return line.trim()
                    .splitToSequence(" ")
                    .drop(1) // drop "v"
                    .map { it.toInt() > 0 }
                    .toList()
                    .toBooleanArray()
            }
            "UNSAT" -> {
                // log.failure("UNSAT in %.2f s".format(timeSolve))
                return null
            }
            else -> {
                // log.error("Implicit UNSAT or ERROR ('$answer') in %.2f s.".format(timeSolve))
                return null
            }
        }
    }

    override fun finalize2() {
        process.destroy()
    }
}
