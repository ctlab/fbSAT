package ru.ifmo.fbsat.core.solver

import com.soywiz.klock.PerformanceCounter
import com.soywiz.klock.measureTimeWithResult
import okio.Buffer
import okio.buffer
import okio.sink
import okio.source
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.write
import ru.ifmo.fbsat.core.utils.writeln

// TODO: make solver able to reset

abstract class Solver {
    var numberOfVariables: Int = 0
        private set
    var numberOfClauses: Int = 0
        private set

    fun newLiteral(): Literal = ++numberOfVariables

    fun <T> newDomainVar(
        domain: Iterable<T>,
        encoding: VarEncoding = Globals.defaultVarEncoding,
        init: (T) -> Literal = { newLiteral() }
    ): DomainVar<T> = DomainVar.create(domain, this, encoding, init)

    fun newIntVar(
        domain: Iterable<Int>,
        encoding: VarEncoding = Globals.defaultVarEncoding,
        init: (Int) -> Literal = { newLiteral() }
    ): IntVar = IntVar.create(domain, this, encoding, init)

    fun newBoolVarArray(
        vararg shape: Int,
        init: (IntArray) -> Literal = { newLiteral() }
    ): BoolVarArray = BoolVarArray.create(shape, init)

    fun <T> newDomainVarArray(
        vararg shape: Int,
        encoding: VarEncoding = Globals.defaultVarEncoding,
        domain: (IntArray) -> Iterable<T>
    ): DomainVarArray<T> = DomainVarArray.create(shape) { index -> newDomainVar(domain(index), encoding) }

    fun newIntVarArray(
        vararg shape: Int,
        encoding: VarEncoding = Globals.defaultVarEncoding,
        domain: (IntArray) -> Iterable<Int>
    ): IntVarArray = IntVarArray.create(shape) { index -> newIntVar(domain(index), encoding) }

    abstract fun comment(comment: String)

    @Suppress("FunctionName")
    protected abstract fun _clause(literals: List<Literal>)

    fun clause(literals: Iterable<Literal>) {
        val pool = literals.filter { it != falseVariable }
        if (trueVariable in pool) return
        if (pool.isNotEmpty()) {
            ++numberOfClauses
            _clause(pool)
        }
    }

    fun clause(vararg literals: Literal): Unit = clause(literals.asIterable())
    fun clause(literals: Sequence<Literal>): Unit = clause(literals.asIterable())
    fun clause(block: suspend SequenceScope<Literal>.() -> Unit): Unit = clause(sequence(block))

    abstract fun solve(): RawAssignment?

    abstract fun finalize2()

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
            solve: () -> RawAssignment? = { TODO() },
            finalize2: () -> Unit = {}
        ): Solver = object : Solver() {
            override fun comment(comment: String) = comment(comment)
            override fun _clause(literals: List<Literal>) = _clause(literals)
            override fun solve(): RawAssignment? = solve()
            override fun finalize2() = finalize2()
        }
    }
}

private class DefaultSolver(private val command: String) : Solver() {
    private val buffer = Buffer()

    override fun comment(comment: String) {
        log.debug { "// $comment" }
        buffer.write("c ").writeln(comment)
    }

    override fun _clause(literals: List<Literal>) {
        for (x in literals)
            buffer.write(x.toString()).write(" ")
        buffer.writeln("0")
    }

    override fun solve(): RawAssignment? {
        val process = Runtime.getRuntime().exec(command)
        val processInput = process.outputStream.sink().buffer()
        processInput.writeln("p cnf $numberOfVariables $numberOfClauses")
        buffer.copyTo(processInput.buffer)

        log.debug { "Solving..." }
        val timeStart = PerformanceCounter.reference
        processInput.close()

        var isSat: Boolean? = null
        val answer: MutableList<Boolean> = mutableListOf()

        process.inputStream.bufferedReader().useLines { lines ->
            label@ for (line in lines.map(String::trim)) {
                when {
                    line == "s SATISFIABLE" -> {
                        val solvingTime = timeSince(timeStart)
                        log.success("SAT in %.2f seconds".format(solvingTime.seconds))
                        isSat = true
                    }
                    line == "s UNSATISFIABLE" -> {
                        val solvingTime = timeSince(timeStart)
                        log.failure("UNSAT in %.2f seconds".format(solvingTime.seconds))
                        isSat = false
                        continue@label
                    }
                    line.startsWith("v ") -> {
                        answer.addAll(
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
            true -> RawAssignment(answer.toBooleanArray())
            false -> null
            null -> error("Implicit UNSAT or ERROR")
        }
    }

    override fun finalize2() {}
}

private class IncrementalSolver(command: String) : Solver() {
    private val process = Runtime.getRuntime().exec(command)
    private val processInput = process.outputStream.sink().buffer()
    private val processOutput = process.inputStream.source().buffer()
    private val buffer = Buffer()

    override fun comment(comment: String) {
        log.debug { "// $comment" }
        processInput.write("c ").writeln(comment)
        buffer.write("c ").writeln(comment)
    }

    override fun _clause(literals: List<Literal>) {
        for (x in literals)
            processInput.write(x.toString()).write(" ")
        processInput.writeln("0")

        for (x in literals)
            buffer.write(x.toString()).write(" ")
        buffer.writeln("0")
    }

    override fun solve(): RawAssignment? {
        processInput.writeln("solve 0")
        processInput.flush()

        log.debug { "Solving..." }
        val (answer, solvingTime) = measureTimeWithResult { processOutput.readUtf8Line() }
        log.debug { "Done solving in %.2f seconds".format(solvingTime.seconds) }

        if (answer == null) {
            // log.error("Solver returned nothing")
            return null
        }

        when (answer) {
            "SAT" -> {
                // log.success("SAT in %.2f s.".format(solvingTime))
                val line = processOutput.readUtf8Line() ?: return null
                return line.trim()
                    .splitToSequence(" ")
                    .drop(1) // drop "v"
                    .map { it.toInt() > 0 }
                    .toList()
                    .toBooleanArray()
                    .let { RawAssignment(it) }
            }
            "UNSAT" -> {
                // log.failure("UNSAT in %.2f s.".format(solvingTime))
                return null
            }
            else -> {
                // log.error("Implicit UNSAT or ERROR ('$answer') in %.2f s.".format(solvingTime))
                return null
            }
        }
    }

    override fun finalize2() {
        process.destroy()
    }
}
