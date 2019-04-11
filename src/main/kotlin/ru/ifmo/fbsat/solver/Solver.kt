package ru.ifmo.fbsat.solver

import okio.Buffer
import okio.buffer
import okio.sink
import okio.source
import ru.ifmo.fbsat.utils.log
import ru.ifmo.fbsat.utils.timeIt
import ru.ifmo.multiarray.IntMultiArray
import kotlin.math.absoluteValue

interface Solver {
    val numberOfVariables: Int
    val numberOfClauses: Int

    fun newVariable(): Int
    fun newArray(vararg shape: Int) = IntMultiArray(shape) { newVariable() }

    fun clause(literals: List<Int>)
    fun clause(vararg literals: Int) = clause(literals.asList())
    fun clause(literals: Sequence<Int>) = clause(literals.toList())
    fun clause(block: suspend SequenceScope<Int>.() -> Unit) = clause(sequence(block))

    fun comment(comment: String)

    fun solve(): BooleanArray?
    fun finalize2()
}

abstract class AbstractSolver : Solver {
    final override var numberOfVariables = 0
        protected set
    final override var numberOfClauses = 0
        protected set

    final override fun newVariable(): Int = ++numberOfVariables
}

class DefaultSolver(private val command: String) : AbstractSolver() {
    private val buffer = Buffer()

    override fun clause(literals: List<Int>) {
        ++numberOfClauses
        for (x in literals)
            buffer.writeUtf8(x.toString()).writeUtf8(" ")
        buffer.writeUtf8("0\n")
    }

    override fun comment(comment: String) {
        log.debug { "// $comment" }
        buffer.writeUtf8("c ").writeUtf8(comment).writeUtf8("\n")
    }

    override fun solve(): BooleanArray? {
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

class IncrementalSolver(command: String) : AbstractSolver() {
    private val process = Runtime.getRuntime().exec(command)
    private val processInput = process.outputStream.sink().buffer()
    private val processOutput = process.inputStream.source().buffer()
    private val buffer = Buffer()

    override fun clause(literals: List<Int>) {
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

    override fun solve(): BooleanArray? {
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
        // Thread {
        //     processInput.write("halt\n")
        //     processInput.flush()
        //     process.waitFor(100, TimeUnit.MILLISECONDS)
        process.destroy()
        // }.start()
    }
}
