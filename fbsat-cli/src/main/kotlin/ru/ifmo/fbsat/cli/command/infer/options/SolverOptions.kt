@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.options

import com.github.ajalt.clikt.core.ParameterHolder
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.switch
import com.github.ajalt.clikt.parameters.options.validate
import com.github.ajalt.clikt.parameters.types.double
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import com.github.lipen.satlib.solver.CadicalSolver
import com.github.lipen.satlib.solver.CryptoMiniSatSolver
import com.github.lipen.satlib.solver.DimacsFileSolver
import com.github.lipen.satlib.solver.DimacsStreamSolver
import com.github.lipen.satlib.solver.GlucoseSolver
import com.github.lipen.satlib.solver.MiniSatSolver
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.solver.IncrementalCryptominisatSolver
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import java.io.File

private val logger = MyLogger {}

internal const val SOLVER_OPTIONS = "Solver Options"

@Suppress("MemberVisibilityCanBePrivate")
class SolverOptions : OptionGroup(SOLVER_OPTIONS) {
    val solverBackend: SolverBackend by solverBackendOption()
    val fileSolverCmd: String by fileSolverCmdOption()
    val fileSolverFile: File by fileSolverFileOption()
    val streamSolverCmd: String by streamSolverCmdOption()
    val solverSeed: Int? by solverSeedOption().validate {
        require(
            solverBackend in listOf(
                SolverBackend.MINISAT,
                SolverBackend.GLUCOSE,
                SolverBackend.CADICAL
            )
        ) { "supported only by MiniSat, Glucose and Cadical" }
    }

    // MiniSat/Glucose options:
    val solverRndFreq: Double? by solverRndFreqOption().validate {
        require(solverBackend in listOf(SolverBackend.MINISAT, SolverBackend.GLUCOSE)) {
            "supported only by MiniSat/Glucose"
        }
        require(it >= 0) { "value must be non-negative" }
    }
    val solverRndPol: Boolean? by solverRndPolOption()
    val solverRndInit: Boolean? by solverRndInitOption()

    // CMS options:
    val solverThreads: Int? by solverThreadsOption().validate {
        require(solverBackend == SolverBackend.CRYPTOMINISAT) {
            "supported only by CryptoMiniSat"
        }
        require(it >= 0) { "value must be non-negative" }
    }

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
                logger.debug { "MiniSat: seed = $solverSeed" }
                logger.debug { "MiniSat: rnd-freq = $solverRndFreq" }
                logger.debug { "MiniSat: rnd-pol = $solverRndPol" }
                logger.debug { "MiniSat: rnd-init = $solverRndInit" }
                MiniSatSolver(
                    simpStrategy = Globals.MINISAT_SIMP_STRATEGY ?: MiniSatSolver.Companion.SimpStrategy.ONCE,
                    initialSeed = solverSeed?.toDouble(),
                    initialRandomVarFreq = solverRndFreq,
                    initialRandomPolarities = solverRndPol ?: false,
                    initialRandomInitialActivities = solverRndInit ?: false
                )
            }
            SolverBackend.GLUCOSE -> {
                logger.debug { "Glucose: seed = $solverSeed" }
                logger.debug { "Glucose: rnd-freq = $solverRndFreq" }
                logger.debug { "Glucose: rnd-pol = $solverRndPol" }
                logger.debug { "Glucose: rnd-init = $solverRndInit" }
                GlucoseSolver(
                    simpStrategy = Globals.GLUCOSE_SIMP_STRATEGY ?: GlucoseSolver.Companion.SimpStrategy.ONCE,
                    initialSeed = solverSeed?.toDouble(),
                    initialRandomVarFreq = solverRndFreq,
                    initialRandomPolarities = solverRndPol ?: false,
                    initialRandomInitialActivities = solverRndInit ?: false
                )
            }
            SolverBackend.CRYPTOMINISAT -> {
                val t = solverThreads
                if (t == null) {
                    CryptoMiniSatSolver()
                } else {
                    logger.debug { "CMS: threads = $t" }
                    CryptoMiniSatSolver(t)
                }
            }
            SolverBackend.CADICAL -> {
                logger.debug { "Cadical: seed = $solverSeed" }
                CadicalSolver(initialSeed = solverSeed)
            }
        }
    }
}

enum class SolverBackend {
    FILE, STREAM, ICMS, MINISAT, GLUCOSE, CRYPTOMINISAT, CADICAL;
}

fun ParameterHolder.solverBackendOption() =
    option(
        help = "SAT-solver backend"
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
        help = "FileSolver CNF file",
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

fun ParameterHolder.solverSeedOption() =
    option(
        "--solver-seed",
        help = "Random seed for SAT solver",
        metavar = "<int>"
    ).int()

fun ParameterHolder.solverRndFreqOption() =
    option(
        "--solver-rnd-freq",
        help = "MiniSat/Glucose random_var_freq",
        metavar = "<dbl>"
    ).double()

fun ParameterHolder.solverRndPolOption() =
    option(
        help = "MiniSat/Glucose rnd_pol"
    ).switch(
        "--solver-rnd-pol" to true,
        "--solver-no-rnd-pol" to false
    )

fun ParameterHolder.solverRndInitOption() =
    option(
        help = "MiniSat/Glucose rnd_init_act"
    ).switch(
        "--solver-rnd-init" to true,
        "--solver-no-rnd-init" to false
    )

fun ParameterHolder.solverThreadsOption() =
    option(
        "--solver-threads",
        help = "CMS threads",
        metavar = "<int>"
    ).int()
