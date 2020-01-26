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
import java.io.File

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

        @JvmStatic
        fun filesolver(file: File, command: String): Solver = FileSolver(file, command)

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
        buffer.writeUtf8("c ").writeUtf8(comment).writeUtf8("\n")
    }

    override fun _clause(literals: List<Literal>) {
        for (x in literals)
            buffer.writeUtf8(x.toString()).writeUtf8(" ")
        buffer.writeUtf8("0\n")
    }

    override fun solve(): RawAssignment? {
        val process = Runtime.getRuntime().exec(command)
        val processInput = process.outputStream.sink().buffer()
        processInput.writeUtf8("p cnf $numberOfVariables $numberOfClauses\n")
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

    override fun solve(): RawAssignment? {
        processInput.writeUtf8("solve 0\n")
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

private class FileSolver(
    val file: File,
    val command: String
) : Solver() {
    private val buffer = Buffer()

    override fun comment(comment: String) {
        log.debug { "// $comment" }
        // buffer.writeUtf8("c ").writeUtf8(comment).writeUtf8("\n")
    }

    override fun _clause(literals: List<Literal>) {
        for (x in literals)
            buffer.writeUtf8(x.toString()).writeUtf8(" ")
        buffer.writeUtf8("0\n")
    }

    override fun solve(): RawAssignment? {
        log.debug { "Solving..." }
        val timeStart = PerformanceCounter.reference

        file.sink().buffer().use {
            it.writeUtf8("p cnf $numberOfVariables $numberOfClauses\n")
            buffer.copyTo(it.buffer)
        }
        log.debug { "Written CNF to '$file' in %.2f s.".format(timeSince(timeStart).seconds) }

        val process = Runtime.getRuntime().exec(command.format(file))
        val reader = process.inputStream.bufferedReader()
        val answer = reader.readLine()

        return when {
            answer.contains("unsat", ignoreCase = true) -> {
                log.failure("UNSAT in %.2f seconds".format(timeSince(timeStart).seconds))
                null
            }
            answer.contains("sat", ignoreCase = true) -> {
                log.success("SAT in %.2f seconds".format(timeSince(timeStart).seconds))
                reader.lineSequence()
                    .map { it.trim() }
                    .flatMap { it.splitToSequence(' ') }
                    .mapNotNull { it.toIntOrNull() }
                    .takeWhile { it != 0 }
                    .map { it > 0 }
                    .let { RawAssignment(it.toList().toBooleanArray()) }
            }
            else -> {
                log.failure("answer = '$answer'")
                error("Implicit UNSAT or ERROR")
            }
        }.also {
            // reader.close()
            process.destroy()
        }
    }

    override fun finalize2() {
        // Do nothing
    }
}
