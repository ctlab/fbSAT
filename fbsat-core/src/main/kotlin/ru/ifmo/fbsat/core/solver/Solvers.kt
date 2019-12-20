package ru.ifmo.fbsat.core.solver

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
import com.soywiz.klock.DateTime
import okio.Buffer
import okio.buffer
import okio.sink
import okio.source
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.secondsSince
import ru.ifmo.fbsat.core.utils.timeIt
import kotlin.math.absoluteValue

// TODO: make solver able to reset

interface Solver {
    val numberOfVariables: Int
    val numberOfClauses: Int

    fun newVariable(): Int

    fun newBoolVar(
        vararg shape: Int,
        init: (IntArray) -> Int = { newVariable() }
    ): BoolVar

    fun newIntVar(
        vararg shape: Int,
        init: (IntArray, Int) -> Int = { _, _ -> newVariable() },
        domain: (IntArray) -> Iterable<Int>
    ): IntVar

    fun <T> newVar(
        vararg shape: Int,
        init: (IntArray, T) -> Int = { _, _ -> newVariable() },
        domain: (IntArray) -> Iterable<T>
    ): Var<T>

    fun comment(comment: String)

    fun clause(literals: Iterable<Int>)
    fun clause(vararg literals: Int)
    fun clause(literals: Sequence<Int>)
    fun clause(block: suspend SequenceScope<Int>.() -> Unit)

    fun solve(): RawAssignment?
    fun finalize2()

    companion object {
        const val trueVariable: Int = Int.MAX_VALUE
        const val falseVariable: Int = -trueVariable

        fun default(command: String): Solver = DefaultSolver(command)
        fun incremental(command: String): Solver = IncrementalSolver(command)
        fun mock(): Solver = object : AbstractSolver() {
            override fun comment(comment: String) = TODO()
            override fun _clause(literals: Iterable<Int>) = TODO()
            override fun _solve(): BooleanArray? = TODO()
            override fun finalize2() = TODO()
        }
    }
}

private abstract class AbstractSolver : Solver {
    final override var numberOfVariables = 0
        protected set
    final override var numberOfClauses = 0
        protected set

    final override fun newVariable(): Int = ++numberOfVariables

    override fun newBoolVar(
        vararg shape: Int,
        init: (IntArray) -> Int
    ): BoolVar = BoolVar.create(shape, init)

    override fun newIntVar(
        vararg shape: Int,
        init: (IntArray, Int) -> Int,
        domain: (IntArray) -> Iterable<Int>
    ): IntVar = IntVar.create(shape, init, { exactlyOne(values) }, domain)

    override fun <T> newVar(
        vararg shape: Int,
        init: (IntArray, T) -> Int,
        domain: (IntArray) -> Iterable<T>
    ): Var<T> = Var.create(shape, init, { exactlyOne(values) }, domain)

    @Suppress("FunctionName")
    protected abstract fun _clause(literals: Iterable<Int>)

    final override fun clause(literals: Iterable<Int>) {
        if (Solver.trueVariable in literals) return

        val lits = literals.filter { it != Solver.falseVariable }
        if (lits.isNotEmpty()) {
            ++numberOfClauses
            _clause(lits)
        }
    }

    final override fun clause(vararg literals: Int) = clause(literals.asIterable())
    final override fun clause(literals: Sequence<Int>) = clause(literals.toList())
    final override fun clause(block: suspend SequenceScope<Int>.() -> Unit) = clause(sequence(block).constrainOnce())

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

    override fun _clause(literals: Iterable<Int>) {
        for (x in literals)
            buffer.writeUtf8(x.toString()).writeUtf8(" ")
        buffer.writeUtf8("0\n")
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
        val timeSolveStart = DateTime.now()
        processInput.close()

        var isSat: Boolean? = null
        val rawAssignment: MutableList<Boolean> = mutableListOf()

        process.inputStream.bufferedReader().useLines { lines ->
            label@ for (line in lines.map(String::trim)) {
                // if (!line.startsWith("v ")) println(line)
                when {
                    line == "s SATISFIABLE" -> {
                        val timeSolve = secondsSince(timeSolveStart)
                        log.success("SAT in %.2f seconds".format(timeSolve))
                        isSat = true
                    }
                    line == "s UNSATISFIABLE" -> {
                        val timeSolve = secondsSince(timeSolveStart)
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

    override fun comment(comment: String) {
        log.debug { "// $comment" }
        processInput.writeUtf8("c ").writeUtf8(comment).writeUtf8("\n")
        buffer.writeUtf8("c ").writeUtf8(comment).writeUtf8("\n")
    }

    override fun _clause(literals: Iterable<Int>) {
        for (x in literals)
            processInput.writeUtf8(x.toString()).writeUtf8(" ")
        processInput.writeUtf8("0\n")

        for (x in literals)
            buffer.writeUtf8(x.toString()).writeUtf8(" ")
        buffer.writeUtf8("0\n")
    }

    override fun _solve(): BooleanArray? {
        // clause(newVariable())

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
