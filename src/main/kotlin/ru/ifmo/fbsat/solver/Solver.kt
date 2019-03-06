package ru.ifmo.fbsat.solver

import ru.ifmo.multiarray.IntMultiArray
import java.io.ByteArrayOutputStream
import java.io.File
import kotlin.math.absoluteValue

interface Solver {
    val numberOfVariables: Int
    val numberOfClauses: Int

    fun newVariable(): Int
    fun newArray(vararg shape: Int) = IntMultiArray(shape) { newVariable() }

    fun clause(literals: Sequence<Int>)
    fun clause(literals: Iterable<Int>) = clause(literals.asSequence())
    fun clause(vararg literals: Int) = clause(literals.asSequence())
    fun clause(block: suspend SequenceScope<Int>.() -> Unit) = clause(sequence(block))

    fun comment(comment: String)

    fun solve(): BooleanArray?
    fun finalize()
}

abstract class AbstractSolver : Solver {
    final override var numberOfVariables = 0
        protected set
    final override var numberOfClauses = 0
        protected set

    override fun newVariable(): Int = ++numberOfVariables
}

class DefaultSolver(private val command: String) : AbstractSolver() {
    private val buffer = ByteArrayOutputStream()
    private val writer = buffer.bufferedWriter()

    override fun clause(literals: Sequence<Int>) {
        ++numberOfClauses
        val s = literals.joinToString(" ", postfix = " 0\n")
        writer.write(s)
    }

    override fun comment(comment: String) {
        println("// $comment")
        val s = "c $comment\n"
        writer.write(s)
    }

    override fun solve(): BooleanArray? {
        // println("[*] Flushing writer...")
        writer.flush()
        // println("[*] Closing writer...")
        // writer.close()

        println("[*] Dumping cnf to file...")
        File("cnf").outputStream().use {
            it.write("p cnf $numberOfVariables $numberOfClauses\n".toByteArray())
            buffer.writeTo(it)
        }

        val process = Runtime.getRuntime().exec(command)
        // println("[*] Writing DIMACS header to process.outputStream...")
        process.outputStream.write("p cnf $numberOfVariables $numberOfClauses\n".toByteArray())
        // println("[*] Redirecting buffer to process.outputStream...")
        buffer.writeTo(process.outputStream)

        println("[*] Solving...")
        val timeSolveStart = System.currentTimeMillis()
        process.outputStream.close()
        val timeSolve = (System.currentTimeMillis() - timeSolveStart) / 1000.0

        var isSat: Boolean? = null
        val rawAssignment: MutableList<Boolean> = mutableListOf()

        process.inputStream.bufferedReader().useLines { lines ->
            label@ for (line in lines.map(String::trim)) {
                // if (!line.startsWith("v ")) println(line)
                when {
                    line == "s SATISFIABLE" -> {
                        println("[+] SAT in %.2f seconds".format(timeSolve))
                        isSat = true
                    }
                    line == "s UNSATISFIABLE" -> {
                        println("[-] UNSAT in %.2f seconds".format(timeSolve))
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

    override fun finalize() {}
}

class IncrementalSolver(command: String) : AbstractSolver() {
    private val process = Runtime.getRuntime().exec(command)
    private val processInput = process.outputStream.bufferedWriter()
    private val processOutput = process.inputStream.bufferedReader()
    // ===
    private val buffer = ByteArrayOutputStream()
    private val writer = buffer.bufferedWriter()

    override fun clause(literals: Sequence<Int>) {
        ++numberOfClauses
        val s = literals.joinToString(" ", postfix = " 0\n")
        processInput.write(s)
        // ===
        writer.write(s)
    }

    override fun comment(comment: String) {
        // println("// $comment")
        val s = "c $comment\n"
        processInput.write(s)
        // ===
        writer.write(s)
    }

    override fun solve(): BooleanArray? {
        writer.flush()
        println("[*] Dumping cnf to file...")
        File("cnf").outputStream().use {
            it.write("p cnf $numberOfVariables $numberOfClauses\n".toByteArray())
            buffer.writeTo(it)
        }

        processInput.write("solve 0\n")
        processInput.flush()

        println("[*] Solving...")
        val timeSolveStart = System.currentTimeMillis()
        val answer: String? = processOutput.readLine()
        val timeSolve = (System.currentTimeMillis() - timeSolveStart) / 1000.0

        if (answer == null) {
            println("[!] Solver returned nothing")
            return null
        }

        return when (answer) {
            "SAT" -> {
                println("[+] SAT in %.2f s".format(timeSolve))

                val line = processOutput.readLine() ?: run {
                    println("[!] Solver returned no assignment")
                    return null
                }

                line.trim()
                    .splitToSequence(" ")
                    .drop(1) // drop "v"
                    .map { it.toInt() > 0 }
                    .toList()
                    .toBooleanArray()
            }
            "UNSAT" -> {
                println("[-] UNSAT in %.2f s".format(timeSolve))
                null
            }
            else -> {
                println("[!] Implicit UNSAT or ERROR (\"$answer\") in %.2f s.".format(timeSolve))
                null
            }
        }
    }

    override fun finalize() {
        // Thread {
        //     processInput.write("halt\n")
        //     processInput.flush()
        //     process.waitFor(100, TimeUnit.MILLISECONDS)
        process.destroy()
        // }.start()
    }
}
