@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.options

import com.github.ajalt.clikt.core.ParameterHolder
import com.github.ajalt.clikt.output.HelpFormatter.Tags.DEFAULT
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.switch
import com.github.ajalt.clikt.parameters.types.file
import ru.ifmo.fbsat.core.solver.Solver
import java.io.File

internal const val SOLVER_OPTIONS = "Solver Options"

@Suppress("MemberVisibilityCanBePrivate")
class SolverOptions : OptionGroup(SOLVER_OPTIONS) {
    val solverBackend: SolverBackend by solverBackendOption()
    val fileSolverCmd: String by fileSolverCmdOption()
    val fileSolverFile: File by fileSolverFileOption()

    val solver: Solver by lazy {
        when (solverBackend) {
            SolverBackend.FILE -> {
                Solver.filesolver(fileSolverCmd, fileSolverFile)
            }
            SolverBackend.ICMS -> {
                Solver.icms()
            }
            SolverBackend.MINISAT -> {
                Solver.minisat()
            }
            SolverBackend.CADICAL -> {
                Solver.cadical()
            }
        }
    }
}

enum class SolverBackend {
    FILE, ICMS, MINISAT, CADICAL;
}

fun ParameterHolder.solverBackendOption() =
    option(
        help = "SAT-solver backend",
        helpTags = mapOf(DEFAULT to "icms (IncrementalCryptominisat)")
    ).switch(
        "--filesolver" to SolverBackend.FILE,
        "--icms" to SolverBackend.ICMS,
        "--minisat" to SolverBackend.MINISAT,
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

fun createSolver(
    solverBackend: SolverBackend,
    fileSolverCmd: String,
    fileSolverFile: File
): Solver = when (solverBackend) {
    SolverBackend.FILE -> {
        Solver.filesolver(fileSolverCmd, fileSolverFile)
    }
    SolverBackend.ICMS -> {
        Solver.icms()
    }
    SolverBackend.MINISAT -> {
        Solver.minisat()
    }
    SolverBackend.CADICAL -> {
        Solver.cadical()
    }
}
