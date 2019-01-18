package ru.ifmo.fbsat.solver

import ru.ifmo.fbsat.utils.IntMultiArray
import java.io.ByteArrayOutputStream
import java.io.File
import kotlin.math.absoluteValue

interface Solver {
    val numberOfVariables: Int
    val numberOfClauses: Int

    fun newVariable(): Int
    fun addClause(literals: Sequence<Int>)
    fun addComment(comment: String)
    fun solve(): BooleanArray?

    fun newArray(vararg shape: Int) = IntMultiArray(shape) { newVariable() }
    fun addClause(vararg literals: Int) = addClause(literals.asSequence())
    fun finalize() {}
}

class DefaultSolver(private val command: String) : Solver {
    private val buffer = ByteArrayOutputStream()
    private val writer = buffer.bufferedWriter()

    override var numberOfVariables = 0
        private set
    override var numberOfClauses = 0
        private set

    override fun newVariable(): Int = ++numberOfVariables

    override fun addClause(literals: Sequence<Int>) {
        ++numberOfClauses
        val s = literals.joinToString(" ", postfix = " 0\n")
        writer.write(s)
    }

    override fun addComment(comment: String) {
        println("// $comment")
        val s = "c $comment\n"
        writer.write(s)
    }

    override fun solve(): BooleanArray? {
        // println("[*] Flushing writer...")
        writer.flush()
//        println("[*] Closing writer...")
//        writer.close()
        println("[*] Dumping cnf to file...")
        buffer.writeTo(File("cnf").outputStream())

        val process = Runtime.getRuntime().exec(command)
        // println("[*] Redirecting buffer to process.outputStream...")
        buffer.writeTo(process.outputStream)
        println("[*] Solving...")
        val timeStartSolve = System.currentTimeMillis()
        process.outputStream.close()

        var isSat: Boolean? = null
        val rawAssignment: MutableList<Boolean> = mutableListOf()

        process.inputStream.bufferedReader().useLines { lines ->
            label@ for (line in lines.map(String::trim)) {
                // =========
                // if (!line.startsWith("v ")) println(line)
                // =========
                when {
                    line == "s SATISFIABLE" -> {
                        println("[+] SAT in %.2f seconds".format((System.currentTimeMillis() - timeStartSolve) / 1000.0))
                        isSat = true
                    }
                    line == "s UNSATISFIABLE" -> {
                        println("[-] UNSAT in %.2f seconds".format((System.currentTimeMillis() - timeStartSolve) / 1000.0))
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
            null -> throw IllegalStateException("Implicit UNSAT or ERROR")
        }
    }
}

class IncrementalSolver(command: String) : Solver {
    private val process = Runtime.getRuntime().exec(command)
    private val processInput = process.outputStream.bufferedWriter()
    private val processOutput = process.inputStream.bufferedReader()
    // ===
    private val buffer = ByteArrayOutputStream()
    private val writer = buffer.bufferedWriter()

    override var numberOfVariables = 0
        private set
    override var numberOfClauses = 0
        private set

    override fun newVariable(): Int = ++numberOfVariables

    override fun addClause(literals: Sequence<Int>) {
        ++numberOfClauses
        val s = literals.joinToString(" ", postfix = " 0\n")
        processInput.write(s)
        // ===
        writer.write(s)
    }

    override fun addComment(comment: String) {
        println("// $comment")
        val s = "c $comment\n"
        processInput.write(s)
        // ===
        writer.write(s)
//        writer.flush()
//        File("cnf").outputStream().use { buffer.writeTo(it) }
    }

    override fun solve(): BooleanArray? {
        // ===
        println("[*] Dumping cnf to file...")
        writer.flush()
        File("cnf").outputStream().use { buffer.writeTo(it) }
        // ===

        println("[*] Solving...")
        val timeStartSolve = System.currentTimeMillis()
        processInput.write("solve 0\n")
        processInput.flush()

        val answer = processOutput.readLine() ?: run {
            println("[!] Solver returned null")
            return null
        }

        when (answer) {
            "SAT" -> {
                println("[+] SAT in %.2f s".format((System.currentTimeMillis() - timeStartSolve) / 1000.0))
                val line = processOutput.readLine() ?: run {
                    println("[!] Solver returned no assignment")
                    return null
                }
                return line
                    .trim()
                    .splitToSequence(" ")
                    .drop(1) // drop "v"
                    .map { it.toInt() > 0 }
                    .toList()
                    .toBooleanArray()
            }
            "UNSAT" -> {
                println("[-] UNSAT in %.2f s".format((System.currentTimeMillis() - timeStartSolve) / 1000.0))
                return null
            }
            else -> {
                println("[-] Implicit UNSAT or ERROR (\"$answer\")")
                return null
            }
        }
    }

    override fun finalize() {
        processInput.write("halt\n")
        processInput.flush()
        process.waitFor()
        process.destroy()
    }
}
