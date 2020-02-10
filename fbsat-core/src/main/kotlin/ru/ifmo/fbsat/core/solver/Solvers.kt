package ru.ifmo.fbsat.core.solver

import com.github.lipen.jnisat.JMiniSat
import com.soywiz.klock.measureTimeWithResult
import okio.Buffer
import okio.BufferedSource
import okio.buffer
import okio.sink
import okio.source
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.lineSequence
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.write
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

interface Solver : AutoCloseable {
    val numberOfVariables: Int
    val numberOfClauses: Int

    fun newLiteral(): Literal
    fun clause(literals: List<Literal>)
    fun assume(literals: List<Literal>)
    fun comment(comment: String)
    fun solve(): RawAssignment?

    override fun close()

    companion object {
        const val trueVariable: Literal = Int.MAX_VALUE
        const val falseVariable: Literal = -trueVariable

        @JvmStatic
        fun default(command: String): Solver = TODO()

        @JvmStatic
        fun incremental(): Solver = IncrementalCryptominisat()

        @JvmStatic
        fun filesolver(command: String, file: File): Solver = FileSolver(command, file)

        @JvmStatic
        fun native(): Solver = MiniSat()

        fun mock(
            comment: (String) -> Unit = {},
            clause: (List<Literal>) -> Unit = {},
            solve: () -> RawAssignment? = { TODO() },
            close: () -> Unit = {}
        ): Solver = object : AbstractSolver() {
            override fun _comment(comment: String) = comment(comment)
            override fun _clause(literals: List<Literal>) = clause(literals)
            override fun _solve(): RawAssignment? = solve()
            override fun _close(): Unit = close()
        }
    }
}

fun Solver.clause(literals: Iterable<Literal>) {
    val pool = literals.filter { it != Solver.falseVariable }
    if (Solver.trueVariable !in pool && pool.isNotEmpty())
        clause(pool)
}

fun Solver.clause(vararg literals: Literal) {
    clause(literals.asIterable())
}

fun Solver.clause(literals: Sequence<Literal>) {
    clause(literals.asIterable())
}

fun Solver.clause(block: suspend SequenceScope<Literal>.() -> Unit) {
    clause(sequence(block))
}

fun <T> Solver.newDomainVar(
    domain: Iterable<T>,
    encoding: VarEncoding = Globals.defaultVarEncoding,
    init: (T) -> Literal = { newLiteral() }
): DomainVar<T> = DomainVar.create(domain, this, encoding, init)

fun Solver.newIntVar(
    domain: Iterable<Int>,
    encoding: VarEncoding = Globals.defaultVarEncoding,
    init: (Int) -> Literal = { newLiteral() }
): IntVar = IntVar.create(domain, this, encoding, init)

fun Solver.newBoolVarArray(
    vararg shape: Int,
    init: (IntArray) -> Literal = { newLiteral() }
): BoolVarArray = BoolVarArray.create(shape, init)

fun <T> Solver.newDomainVarArray(
    vararg shape: Int,
    encoding: VarEncoding = Globals.defaultVarEncoding,
    domain: (IntArray) -> Iterable<T>
): DomainVarArray<T> = DomainVarArray.create(shape) { index -> newDomainVar(domain(index), encoding) }

fun Solver.newIntVarArray(
    vararg shape: Int,
    encoding: VarEncoding = Globals.defaultVarEncoding,
    domain: (IntArray) -> Iterable<Int>
): IntVarArray = IntVarArray.create(shape) { index -> newIntVar(domain(index), encoding) }

@Suppress("FunctionName")
abstract class AbstractSolver : Solver {
    override var numberOfVariables: Int = 0
        protected set
    final override var numberOfClauses: Int = 0
        private set

    override fun newLiteral(): Literal = ++numberOfVariables

    final override fun clause(literals: List<Literal>) {
        ++numberOfClauses
        _clause(literals)
    }

    final override fun assume(literals: List<Literal>) {
        log.debug { "Assuming $literals" }
        TODO()
    }

    final override fun comment(comment: String) {
        log.debug { "// $comment" }
        _comment(comment)
    }

    final override fun solve(): RawAssignment? {
        log.debug { "Solving..." }
        val (result, solvingTime) = measureTimeWithResult { _solve() }
        log.debug {
            val answer = when {
                result != null -> "SAT"
                else -> "UNSAT"
            }
            "Done solving ($answer) in %.2f seconds".format(solvingTime.seconds)
        }
        return result
    }

    final override fun close() {
        log.debug { "Closing solver..." }
        _close()
    }

    protected abstract fun _clause(literals: List<Literal>)
    protected abstract fun _comment(comment: String)
    protected abstract fun _solve(): RawAssignment?
    protected abstract fun _close()
}

class FileSolver(
    val command: String,
    val file: File
) : AbstractSolver() {
    private val buffer = Buffer()

    override fun _clause(literals: List<Literal>) {
        for (x in literals)
            buffer.write(x.toString()).write(" ")
        buffer.writeln("0")
    }

    override fun _comment(comment: String) {
        buffer.write("c ").writeln(comment)
    }

    override fun _solve(): RawAssignment? {
        file.sink().buffer().use {
            it.writeln("p cnf $numberOfVariables $numberOfClauses")
            buffer.copyTo(it.buffer)
        }

        val process = Runtime.getRuntime().exec(command.format(file))
        val processOutput = process.inputStream.source().buffer()

        return parseDimacsOutput(processOutput)
    }

    override fun _close() {}
}

private fun parseDimacsOutput(source: BufferedSource): RawAssignment? {
    val answer = source.lineSequence().firstOrNull { it.startsWith("s ") }
        ?: error("No answer from solver")
    return when {
        "UNSAT" in answer -> null
        "SAT" in answer -> source
            .lineSequence()
            .filter { it.startsWith("v ") }
            .flatMap { it.drop(2).trim().splitToSequence(' ') }
            .map { it.toInt() }
            .takeWhile { it != 0 }
            .map { it > 0 }
            .toList()
            .toBooleanArray()
            .let {
                check(it.isNotEmpty()) { "No model from solver for SAT" }
                RawAssignment(it)
            }
        else -> error("Bad answer (neither SAT nor UNSAT) from solver: '$answer'")
    }
}

class IncrementalCryptominisat : AbstractSolver() {
    private val process = Runtime.getRuntime().exec("incremental-cryptominisat")
    private val processInput = process.outputStream.sink().buffer()
    private val processOutput = process.inputStream.source().buffer()
    private val buffer = Buffer()

    override fun _comment(comment: String) {
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

    override fun _solve(): RawAssignment? {
        processInput.writeln("solve 0").flush()

        if (Globals.IS_DEBUG) {
            // Dump intermediate cnf
            File("cnf").sink().buffer().use {
                it.writeln("p cnf $numberOfVariables $numberOfClauses")
                buffer.copyTo(it.buffer)
            }
        }

        return parseIcmsOutput(processOutput)
    }

    override fun _close() {
        // processInput.writeln("halt")
        process.destroy()
    }
}

private fun parseIcmsOutput(source: BufferedSource): RawAssignment? =
    when (val answer = source.readUtf8Line() ?: error("Solver returned nothing")) {
        "SAT" -> {
            val line = source.readUtf8Line() ?: error("No model from solver for SAT")
            line.trim()
                .splitToSequence(' ')
                .drop(1) // drop starting "v"
                .map { it.toInt() }
                .takeWhile { it != 0 }
                .map { it > 0 }
                .toList()
                .toBooleanArray()
                .let {
                    check(it.isNotEmpty()) { "No model from solver for SAT" }
                    RawAssignment(it)
                }
        }
        "UNSAT" -> null
        else -> error("Bad answer (neither SAT nor UNSAT) from solver: '$answer'")
    }

class MiniSat : AbstractSolver() {
    private val backend = JMiniSat()
    private val buffer = Buffer()

    override fun newLiteral(): Literal {
        ++numberOfVariables
        return backend.addVariable()
    }

    override fun _clause(literals: List<Literal>) {
        for (x in literals)
            buffer.write(x.toString()).write(" ")
        buffer.writeln("0")

        when (literals.size) {
            1 -> backend.addClause(literals[0])
            2 -> backend.addClause(literals[0], literals[1])
            3 -> backend.addClause(literals[0], literals[1], literals[2])
            else -> backend.addClause(*literals.toIntArray())
        }
    }

    override fun _comment(comment: String) {
        buffer.write("c ").writeln(comment)
    }

    override fun _solve(): RawAssignment? {
        buffer.writeln("c solve")

        if (Globals.IS_DEBUG) {
            // Dump intermediate cnf
            File("cnf").sink().buffer().use {
                it.writeln("p cnf $numberOfVariables $numberOfClauses")
                buffer.copyTo(it.buffer)
            }
        }

        if (!backend.solve()) return null
        val model = BooleanArray(numberOfVariables) { i -> backend.getValue(i + 1) > 0 }
        return RawAssignment(model)
    }

    override fun _close() {
        backend.close()
    }
}
