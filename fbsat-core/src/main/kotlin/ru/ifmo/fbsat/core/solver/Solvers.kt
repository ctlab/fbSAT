package ru.ifmo.fbsat.core.solver

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import okio.Buffer
import okio.buffer
import okio.sink
import okio.source
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeIt
import kotlin.math.absoluteValue
import kotlin.properties.ReadOnlyProperty
import kotlin.reflect.KProperty

class SolverContext internal constructor(val solver: Solver) : MutableMap<String, Any> by mutableMapOf() {
    operator fun <T : Any> invoke(value: T): ContextProvider<T> = ContextProvider(value)
    // operator fun <T : Any> invoke(init: Solver.() -> T): ContextProvider<T> = invoke(solver.init())

    inner class ContextProvider<T : Any>(val value: T) {
        operator fun provideDelegate(thisRef: Any?, property: KProperty<*>): ReadOnlyProperty<Any?, T> {
            this@SolverContext[property.name] = value
            return object : ReadOnlyProperty<Any?, T> {
                override fun getValue(thisRef: Any?, property: KProperty<*>): T {
                    // To return dynamic value, use this:
                    // return this@SolverContext.getForce(property.name)
                    return value
                }
            }
        }
    }
}

class RawAssignment(
    private val data: BooleanArray,
    private val context: SolverContext
) : Map<String, Any> by context {
    operator fun get(index: Int): Boolean = data[index - 1]

    fun booleanArray(
        variable: IntMultiArray,
        vararg shape: Int
    ) = BooleanMultiArray.create(shape) { index ->
        @Suppress("ReplaceGetOrSet")
        this[variable.get(*index)]
    }

    fun intArray(
        variable: IntMultiArray,
        vararg shape: Int,
        domain: Iterable<Int>,
        onAbsence: (index: IntArray) -> Int /*= { error("variable[index = $it] is undefined") }*/
    ) = IntMultiArray.create(shape) { index ->
        @Suppress("ReplaceGetOrSet")
        domain.firstOrNull { last -> this[variable.get(*index, last)] }
            ?: onAbsence(index)
    }
}

interface Solver {
    val numberOfVariables: Int
    val numberOfClauses: Int
    val context: SolverContext

    fun newVariable(): Int
    fun newArray(vararg shape: Int, init: (IntArray) -> Int = { newVariable() }): IntMultiArray =
        IntMultiArray.create(shape, init)

    fun clause(literals: List<Int>)
    fun clause(vararg literals: Int) = clause(literals.asList())
    fun clause(literals: Sequence<Int>) = clause(literals.toList())
    fun clause(block: suspend SequenceScope<Int>.() -> Unit) = clause(sequence(block).constrainOnce())

    fun comment(comment: String)

    fun solve(): RawAssignment?
    fun finalize2()

    companion object {
        fun default(command: String): Solver = DefaultSolver(command)
        fun incremental(command: String): Solver = IncrementalSolver(command)
    }
}

private abstract class AbstractSolver : Solver {
    final override var numberOfVariables = 0
        protected set
    final override var numberOfClauses = 0
        protected set
    final override val context: SolverContext by lazy { SolverContext(this) }

    final override fun newVariable(): Int = ++numberOfVariables

    @Suppress("FunctionName")
    protected abstract fun _solve(): BooleanArray?

    final override fun solve(): RawAssignment? = _solve()?.let { RawAssignment(it, context) }
}

private class DefaultSolver(private val command: String) : AbstractSolver() {
    private val buffer = Buffer()
    private val falseVariable: Int by context(newVariable())

    init {
        // Note: cannot just use "clause(-falseVariable)" because it drops clauses with -falseVariable
        buffer.writeUtf8("${-falseVariable} 0\n")
    }

    override fun clause(literals: List<Int>) {
        if (-falseVariable in literals) return
        if (falseVariable in literals) return clause(literals.filter { it != falseVariable })

        ++numberOfClauses
        for (x in literals)
            buffer.writeUtf8(x.toString()).writeUtf8(" ")
        buffer.writeUtf8("0\n")
    }

    override fun comment(comment: String) {
        log.debug { "// $comment" }
        buffer.writeUtf8("c ").writeUtf8(comment).writeUtf8("\n")
    }

    override fun _solve(): BooleanArray? {
        // println("[*] Dumping cnf to file...")
        // File("cnf").outputStream().use {
        //     it.write("p cnf $numberOfVariables $numberOfClauses\n".toByteArray())
        //     buffer.copyTo(it)
        // }

        val process = Runtime.getRuntime().exec(command)
        val processInput = process.outputStream.sink().buffer()
        // println("[*] Writing DIMACS header to process.outputStream...")
        processInput.writeUtf8("p cnf $numberOfVariables $numberOfClauses\n")
        // println("[*] Redirecting buffer to process.outputStream...")
        buffer.copyTo(processInput.buffer)

        log.debug { "Solving..." }
        val timeSolveStart = System.currentTimeMillis()
        processInput.close()

        var isSat: Boolean? = null
        val rawAssignment: MutableList<Boolean> = mutableListOf()

        process.inputStream.bufferedReader().useLines { lines ->
            label@ for (line in lines.map(String::trim)) {
                // if (!line.startsWith("v ")) println(line)
                when {
                    line == "s SATISFIABLE" -> {
                        val timeSolve = (System.currentTimeMillis() - timeSolveStart) / 1000.0
                        log.success("SAT in %.2f seconds".format(timeSolve))
                        isSat = true
                    }
                    line == "s UNSATISFIABLE" -> {
                        val timeSolve = (System.currentTimeMillis() - timeSolveStart) / 1000.0
                        log.failure("[-] UNSAT in %.2f seconds".format(timeSolve))
                        isSat = false
                        continue@label
                    }
                    line.startsWith("v ") -> {
                        val values = line
                            .splitToSequence(" ")
                            .drop(1) // drop "v"
                            .map { it.toInt() }
                            .takeWhile { it != 0 }
                        values.forEachIndexed { i, v ->
                            require(i + rawAssignment.size + 1 == v.absoluteValue) {
                                "Value $v should be ${i + rawAssignment.size + 1}"
                            }
                        }
                        rawAssignment.addAll(values.map { it > 0 })
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
    private val falseVariable: Int by context(newVariable())

    init {
        // Note: cannot just use "clause(-falseVariable)" because it drops clauses with -falseVariable
        processInput.writeUtf8("${-falseVariable} 0\n")
        buffer.writeUtf8("${-falseVariable} 0\n")
    }

    override fun clause(literals: List<Int>) {
        if (-falseVariable in literals) return
        if (falseVariable in literals) return clause(literals.filter { it != falseVariable })
        // require(literals.isNotEmpty())
        // literals.forEach { check(it != 0) }

        ++numberOfClauses

        for (x in literals)
            processInput.writeUtf8(x.toString()).writeUtf8(" ")
        processInput.writeUtf8("0\n")

        for (x in literals)
            buffer.writeUtf8(x.toString()).writeUtf8(" ")
        buffer.writeUtf8("0\n")
    }

    override fun comment(comment: String) {
        log.debug { "// $comment" }
        processInput.writeUtf8("c ").writeUtf8(comment).writeUtf8("\n")
        buffer.writeUtf8("c ").writeUtf8(comment).writeUtf8("\n")
    }

    override fun _solve(): BooleanArray? {
        // measureNanoTime {
        //     println("[*] Dumping cnf to file...")
        //     File("cnf").outputStream().use {
        //         it.write("p cnf $numberOfVariables $numberOfClauses\n".toByteArray())
        //         buffer.copyTo(it)
        //     }
        // }.also {
        //     println("CNF dumped in %.2f ms".format(it / 1_000_000.0))
        // }

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
