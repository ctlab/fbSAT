package ru.ifmo.fbsat.core.solver

import com.github.lipen.jnisat.JCadical
import com.github.lipen.jnisat.JMiniSat
import com.soywiz.klock.measureTimeWithResult
import okio.Buffer
import okio.BufferedSink
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
import kotlin.properties.ReadOnlyProperty
import kotlin.reflect.KProperty

class SolverContext internal constructor(
    val solver: Solver,
    val map: MutableMap<String, Any> = mutableMapOf()
) {
    operator fun <T : Any> invoke(value: T): ContextProvider<T> = ContextProvider(value)
    // inline operator fun <T : Any> invoke(init: Solver.() -> T): ContextProvider<T> = invoke(solver.init())

    operator fun <T> getValue(thisRef: Any?, property: KProperty<*>): T = get(property.name)

    @Suppress("UNCHECKED_CAST")
    operator fun <T> get(key: String): T = map.getValue(key) as T

    operator fun set(key: String, value: Any) {
        map[key] = value
    }

    inner class ContextProvider<T : Any>(val value: T) {
        operator fun provideDelegate(thisRef: Any?, property: KProperty<*>): ReadOnlyProperty<Any?, T> {
            this@SolverContext[property.name] = value
            return object : ReadOnlyProperty<Any?, T> {
                override fun getValue(thisRef: Any?, property: KProperty<*>): T = value
            }
        }
    }
}

@Suppress("FunctionName")
interface Solver : AutoCloseable {
    val context: SolverContext
    val numberOfVariables: Int
    val numberOfClauses: Int

    fun newLiteral(): Literal

    fun comment(comment: String)

    fun clause(vararg literals: Literal)
    fun clause_(literals: IntArray)
    fun clause_(literals: List<Literal>)

    fun solve(): RawAssignment?
    fun reset()

    companion object {
        const val trueLiteral: Literal = Int.MAX_VALUE
        const val falseLiteral: Literal = -trueLiteral

        @JvmStatic
        fun icms(): Solver = IncrementalCryptominisat()

        @JvmStatic
        fun filesolver(command: String, file: File): Solver = FileSolver(command, file)

        @JvmStatic
        fun minisat(): Solver = MiniSat()

        @JvmStatic
        fun cadical(): Solver = Cadical()

        fun mock(
            comment: (String) -> Unit = {},
            clause: (List<Literal>) -> Unit = {},
            solve: () -> RawAssignment? = { TODO() },
            reset: () -> Unit = {},
            close: () -> Unit = {}
        ): Solver = object : AbstractSolver() {
            override fun _comment(comment: String) = comment(comment)
            override fun _clause(literals: IntArray): Unit = _clause(literals.toList())
            override fun _clause(literals: List<Literal>) = clause(literals)
            override fun _solve(): RawAssignment? = solve()
            override fun _reset(): Unit = reset()
            override fun _close(): Unit = close()
        }
    }
}

fun Solver.clause(literals: Sequence<Literal>) {
    clause_(literals.toList())
}

fun Solver.clause(block: suspend SequenceScope<Literal>.() -> Unit) {
    clause(sequence(block))
}

fun Solver.clause(literals: Iterable<Literal>) {
    val pool = literals.filter { it != Solver.falseLiteral }
    if (Solver.trueLiteral !in pool && pool.isNotEmpty())
        clause_(pool)
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
    @Suppress("LeakingThis")
    final override val context: SolverContext = SolverContext(this)
    override var numberOfVariables: Int = 0
        protected set
    final override var numberOfClauses: Int = 0
        private set

    override fun newLiteral(): Literal = ++numberOfVariables

    final override fun comment(comment: String) {
        // log.debug { "// $comment" }
        _comment(comment)
    }

    final override fun clause(vararg literals: Literal) {
        clause_(literals)
    }

    final override fun clause_(literals: IntArray) {
        ++numberOfClauses
        _clause(literals)
    }

    final override fun clause_(literals: List<Literal>) {
        ++numberOfClauses
        _clause(literals)
    }

    final override fun solve(): RawAssignment? {
        log.debug { "Solving..." }
        val (result, solvingTime) = measureTimeWithResult { _solve() }
        log.debug {
            val answer = when {
                result != null -> "SAT"
                else -> "UNSAT"
            }
            "Done solving ($answer) in %.3f seconds".format(solvingTime.seconds)
        }
        return result
    }

    final override fun reset() {
        log.debug { "Resetting solver..." }
        numberOfVariables = 0
        numberOfClauses = 0
        context.map.clear()
        _reset()
    }

    final override fun close() {
        log.debug { "Closing solver..." }
        _close()
    }

    protected abstract fun _clause(literals: IntArray)
    protected abstract fun _clause(literals: List<Literal>)
    protected abstract fun _comment(comment: String)
    protected abstract fun _solve(): RawAssignment?
    protected abstract fun _reset()
    protected abstract fun _close()
}

class FileSolver(
    val command: String,
    val file: File
) : AbstractSolver() {
    private val buffer = Buffer()

    override fun _comment(comment: String) {
        buffer.write("c ").writeln(comment)
    }

    override fun _clause(literals: IntArray) {
        _clause(literals.toList())
    }

    override fun _clause(literals: List<Literal>) {
        for (x in literals)
            buffer.write(x.toString()).write(" ")
        buffer.writeln("0")
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

    override fun _reset() {
        buffer.clear()
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
                RawAssignment0(it)
            }
        else -> error("Bad answer (neither SAT nor UNSAT) from solver: '$answer'")
    }
}

class IncrementalCryptominisat : AbstractSolver() {
    private lateinit var process: Process
    private lateinit var processInput: BufferedSink
    private lateinit var processOutput: BufferedSource
    private val buffer = Buffer()
    private var isInitialized = false

    init {
        _reset()
        isInitialized = true
    }

    override fun _comment(comment: String) {
        processInput.write("c ").writeln(comment)
        buffer.write("c ").writeln(comment)
    }

    override fun _clause(literals: IntArray) {
        _clause(literals.toList())
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

    override fun _reset() {
        if (isInitialized) process.destroy()
        process = Runtime.getRuntime().exec("incremental-cryptominisat")
        processInput = process.outputStream.sink().buffer()
        processOutput = process.inputStream.source().buffer()
        buffer.clear()
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
                    RawAssignment0(it)
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
        return backend.newVariable()
    }

    override fun _comment(comment: String) {
        buffer.write("c ").writeln(comment)
    }

    override fun _clause(literals: IntArray) {
        for (x in literals)
            buffer.write(x.toString()).write(" ")
        buffer.writeln("0")

        backend.addClause_(literals.toIntArray())
    }

    override fun _clause(literals: List<Literal>) {
        _clause(literals.toIntArray())
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
        val model = backend.getModel()
        return RawAssignment1(model)
    }

    override fun _reset() {
        backend.reset()
        buffer.clear()
    }

    override fun _close() {
        backend.close()
    }
}

class Cadical : AbstractSolver() {
    private lateinit var backend: JCadical
    private val buffer = Buffer()

    init {
        _reset()
    }

    override fun newLiteral(): Literal {
        ++numberOfVariables
        return backend.newVariable()
    }

    override fun _comment(comment: String) {
        buffer.write("c ").writeln(comment)
    }

    override fun _clause(literals: IntArray) {
        for (x in literals)
            buffer.write(x.toString()).write(" ")
        buffer.writeln("0")

        backend.addClause_(literals.toIntArray())
    }

    override fun _clause(literals: List<Literal>) {
        _clause(literals.toIntArray())
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
        val model = backend.getModel()
        return RawAssignment1(model)
    }

    fun getValue(lit: Literal): Boolean {
        return backend.getValue(lit)
    }

    fun getModel(): BooleanArray {
        return backend.getModel()
    }

    override fun _reset() {
        // TODO: proper backend.reset()
        backend = JCadical()
        backend.setLongOption("--elim=0")
        buffer.clear()
    }

    override fun _close() {
        backend.close()
    }
}
