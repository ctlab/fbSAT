@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.options

import com.github.ajalt.clikt.core.ParameterHolder
import com.github.ajalt.clikt.output.HelpFormatter.Tags.DEFAULT
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.switch
import com.github.ajalt.clikt.parameters.types.file
import com.github.lipen.satlib.solver.CadicalSolver
import com.github.lipen.satlib.solver.CryptoMiniSatSolver
import com.github.lipen.satlib.solver.DimacsFileSolver
import com.github.lipen.satlib.solver.DimacsStreamSolver
import com.github.lipen.satlib.solver.GlucoseSolver
import com.github.lipen.satlib.solver.MiniSatSolver
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.solver.IncrementalCryptominisatSolver
import ru.ifmo.fbsat.core.utils.Globals
import java.io.File

internal const val SOLVER_OPTIONS = "Solver Options"

@Suppress("MemberVisibilityCanBePrivate")
class SolverOptions : OptionGroup(SOLVER_OPTIONS) {
    val solverBackend: SolverBackend by solverBackendOption()
    val fileSolverCmd: String by fileSolverCmdOption()
    val fileSolverFile: File by fileSolverFileOption()
    val streamSolverCmd: String by streamSolverCmdOption()

    val solver: Solver by lazy {
        when (solverBackend) {
            SolverBackend.FILE -> {
                DimacsFileSolver({ fileSolverFile }, { fileSolverCmd.format(it) })
            }
            SolverBackend.STREAM -> {
                DimacsStreamSolver { streamSolverCmd }
            }
            SolverBackend.ICMS -> {
                IncrementalCryptominisatSolver { Globals.ICMS_CMD }
            }
            SolverBackend.MINISAT -> {
                when (val simp = Globals.MINISAT_SIMP_STRATEGY) {
                    null -> MiniSatSolver()
                    else -> MiniSatSolver(simp)
                }
            }
            SolverBackend.GLUCOSE -> {
                when (val simp = Globals.GLUCOSE_SIMP_STRATEGY) {
                    null -> GlucoseSolver()
                    else -> GlucoseSolver(simp)
                }
            }
            SolverBackend.CRYPTOMINISAT -> {
                CryptoMiniSatSolver()
            }
            SolverBackend.CADICAL -> {
                CadicalSolver()
            }
        }
    }
}

enum class SolverBackend {
    FILE, STREAM, ICMS, MINISAT, GLUCOSE, CRYPTOMINISAT, CADICAL;
}

fun ParameterHolder.solverBackendOption() =
    option(
        help = "SAT-solver backend",
        helpTags = mapOf(DEFAULT to "icms (incremental-cryptominisat)")
    ).switch(
        "--filesolver" to SolverBackend.FILE,
        "--streamsolver" to SolverBackend.STREAM,
        "--icms" to SolverBackend.ICMS,
        "--minisat" to SolverBackend.MINISAT,
        "--glucose" to SolverBackend.GLUCOSE,
        "--cryptominisat" to SolverBackend.CRYPTOMINISAT,
        "--cadical" to SolverBackend.CADICAL
    ).default(
        SolverBackend.ICMS
    )

fun ParameterHolder.fileSolverCmdOption() =
    option(
        "--filesolver-cmd",
        help = "FileSolver command (use %s placeholder to access the filename)",
        metavar = "<cmd>"
    ).default(
        "cadical %s"
    )

fun ParameterHolder.fileSolverFileOption() =
    option(
        "--filesolver-file",
        help = "FileSolver file (for CNF)",
        metavar = "<path>"
    ).file(
        canBeDir = false
    ).default(
        File("cnf")
    )

fun ParameterHolder.streamSolverCmdOption() =
    option(
        "--streamsolver-cmd",
        help = "StreamSolver command",
        metavar = "<cmd>"
    ).default(
        "cadical"
    )
