package ru.ifmo.fbsat.core.solver

import com.github.lipen.satlib.core.Lit
import com.github.lipen.satlib.core.LitArray
import com.github.lipen.satlib.core.Model
import com.github.lipen.satlib.solver.AbstractSolver
import okio.BufferedSink
import okio.BufferedSource
import okio.buffer
import okio.sink
import okio.source
import ru.ifmo.fbsat.core.utils.readLine
import ru.ifmo.fbsat.core.utils.write
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

class IncrementalCryptominisatSolver(
    val command: () -> String = { "incremental-cryptominisat" },
) : AbstractSolver() {
    private lateinit var process: Process
    private lateinit var processInput: BufferedSink
    private lateinit var processOutput: BufferedSource
    private var isInitialized: Boolean = false
    private var _model: Model? = null

    init {
        _reset()
        isInitialized = true
    }

    override fun _reset() {
        if (isInitialized) process.destroy()
        val command = command()
        process = Runtime.getRuntime().exec(command)
        processInput = process.outputStream.sink().buffer()
        processOutput = process.inputStream.source().buffer()
        _model = null
    }

    override fun _close() {
        // processInput.writeln("halt")
        process.destroy()
    }

    override fun _comment(comment: String) {
        for (line in comment.lineSequence()) {
            processInput.write("c ").writeln(line)
        }
    }

    override fun _dumpDimacs(file: File) {
        TODO("IncrementalCryptominisatSolver does not support `dumpDimacs`")
    }

    override fun _newLiteral(outerNumberOfVariables: Int): Lit {
        return outerNumberOfVariables
    }

    override fun _addClause() {
        processInput.writeln("0")
    }

    override fun _addClause(lit: Lit) {
        processInput.writeln("$lit 0")
    }

    override fun _addClause(lit1: Lit, lit2: Lit) {
        processInput.writeln("$lit1 $lit2 0")
    }

    override fun _addClause(lit1: Lit, lit2: Lit, lit3: Lit) {
        processInput.writeln("$lit1 $lit2 $lit3 0")
    }

    override fun _addClause(literals: LitArray) {
        _addClause(literals.asList())
    }

    override fun _addClause(literals: List<Lit>) {
        for (lit in literals) {
            processInput.write(lit.toString()).write(" ")
        }
        processInput.writeln("0")
    }

    override fun _solve(): Boolean {
        processInput.writeln("solve 0").flush()
        _model = parseIcmsOutput(processOutput)
        return _model != null
    }

    override fun _solve(assumptions: LitArray): Boolean {
        throw UnsupportedOperationException(ASSUMPTIONS_NOT_SUPPORTED)
    }

    override fun interrupt() {
        throw UnsupportedOperationException(INTERRUPTION_NOT_SUPPORTED)
    }

    override fun getValue(lit: Lit): Boolean {
        return getModel()[lit]
    }

    override fun getModel(): Model {
        return _model ?: error("Model is null because the solver is not in the SAT state")
    }

    companion object {
        private const val NAME = "IncrementalCryptominisatSolver"
        private const val ASSUMPTIONS_NOT_SUPPORTED: String =
            "$NAME does not support solving with assumptions"
        private const val INTERRUPTION_NOT_SUPPORTED: String =
            "$NAME does not support interruption"
    }
}

private fun parseIcmsOutput(source: BufferedSource): Model? =
    when (val answer = source.readLine() ?: error("Solver returned nothing")) {
        "SAT" -> {
            val line = source.readLine() ?: error("No model from solver for SAT")
            line.trim()
                .splitToSequence(' ')
                .drop(1) // drop starting "v"
                .map { it.toInt() }
                .takeWhile { it != 0 }
                .map { it > 0 }
                .toList()
                .also { check(it.isNotEmpty()) { "Model is empty" } }
                .let { Model.from(it, zerobased = true) }
        }
        "UNSAT" -> null
        else -> error("Bad answer (neither SAT nor UNSAT) from solver: '$answer'")
    }
