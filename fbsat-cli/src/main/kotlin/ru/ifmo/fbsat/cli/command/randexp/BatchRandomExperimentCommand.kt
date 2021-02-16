@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.cli.command.randexp

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.ParameterHolder
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.check
import com.github.ajalt.clikt.parameters.options.convert
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.multiple
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.options.validate
import com.github.ajalt.clikt.parameters.types.choice
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import kotlinx.serialization.json.Json
import ru.ifmo.fbsat.cli.command.infer.options.isDebugOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfStatesOption
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.fbsatSerializersModule
import java.io.File

private val logger = MyLogger {}

private val myJson = Json {
    serializersModule = fbsatSerializersModule
    prettyPrint = true
}

internal const val BATCH_DATA_OPTIONS = "Batch Data Options"
internal const val BATCH_INFERENCE_OPTIONS = "Batch Inference Options"
internal const val BATCH_SOLVER_OPTIONS = "Batch Solver Options"
internal const val BATCH_OPTIONS = "Batch Options"

private fun parseRange(s: String): IntRange =
    if (s.contains("-")) {
        val (min, max) = s.split("-", limit = 2).map(String::toInt)
        min..max
    } else {
        val x = s.toInt()
        x..x
    }

private fun ParameterHolder.numberOfInputVariablesOption() =
    option(
        "-X",
        help = "Number of input variables",
        metavar = "<int>"
    ).int()

private fun ParameterHolder.numberOfOutputVariablesOption() =
    option(
        "-Z",
        help = "Number of output variables",
        metavar = "<int>"
    ).int()

private fun ParameterHolder.maxGuardSizeGenOption() =
    option(
        "-Pgen",
        help = "Maximum guard size in the generated random automaton",
        metavar = "<int>"
    ).int()

private fun ParameterHolder.automatonSeedsOption() =
    option(
        "--as", "--automaton-seeds",
        help = "Seeds for automaton",
        metavar = "<range>"
    ).convert {
        parseRange(it)
    }

private fun ParameterHolder.numberOfScenariosOption() =
    option(
        "-n",
        help = "Number of random scenarios to generate",
        metavar = "<int>"
    ).int()
        .required()
        .check("value must be non-negative") { it >= 0 }

private fun ParameterHolder.scenarioLengthOption() =
    option(
        "-k",
        help = "Length of each scenario",
        metavar = "<int>"
    ).int()
        .required()
        .check("value must be non-negative") { it >= 0 }

private fun ParameterHolder.scenariosSeedsOption() =
    option(
        "--ss", "--scenarios-seeds",
        help = "Seeds for scenarios",
        metavar = "<range>"
    ).convert {
        parseRange(it)
    }

private fun ParameterHolder.methodOption() =
    option(
        "-m", "--method",
        help = "Method of inference",
    ).choice(
        "basic-min",
        "extended-min",
        "extended-min-ub",
    )

private fun ParameterHolder.outBaseDirOption() =
    option(
        "-o", "--outdir",
        help = "Base output directory",
        metavar = "<path>"
    ).file().default(File("out/randexp-batch"))

private fun ParameterHolder.solverOption() =
    option(
        "--solver",
        help = "SAT solver",
    ).choice(
        "minisat",
        "glucose",
        "cadical",
    )

private fun ParameterHolder.solverSeedsOption() =
    option(
        "--rs", "--solver-seeds",
        help = "Random seeds for SAT solver",
        metavar = "<range>"
    ).convert {
        parseRange(it)
    }

private fun ParameterHolder.fbsatBinOption() =
    option(
        "--fbsat-bin",
        help = "fbSAT binary",
        metavar = "<cmd>"
    ).default("fbsat")

private fun ParameterHolder.sbatchBinOption() =
    option(
        "--sbatch-bin",
        help = "sbatch binary",
        metavar = "<cmd>"
    ).default("sbatch")

private fun ParameterHolder.sbatchOptionsOption() =
    option(
        "--opt", "--sbatch-option",
        help = "Custom sbatch option (`#SBATCH <opt>`). Can be passed multiple times",
        metavar = "<str>"
    ).multiple().validate {
        for (opt in it) {
            require(!opt.contains('\n')) { "'$opt' contains newline" }
        }
    }

private fun ParameterHolder.isIgnoreLockOption() =
    option(
        "--ignore-lock",
        help = "Ignore lock files"
    ).flag()

private fun ParameterHolder.isDryRunOption() =
    option(
        "--dry-run",
        help = "Do not run sbatch"
    ).flag()

private class BatchRandomExperimentDataOptions : OptionGroup(BATCH_DATA_OPTIONS) {
    val numberOfStates: Int by numberOfStatesOption().required()
    val numberOfInputVariables: Int by numberOfInputVariablesOption().required()
    val numberOfOutputVariables: Int by numberOfOutputVariablesOption().required()
    val maxGuardSize: Int by maxGuardSizeGenOption().default(5)
    val automatonSeeds: IntRange by automatonSeedsOption().required()
}

private class BatchRandomExperimentInferenceOptions : OptionGroup(BATCH_INFERENCE_OPTIONS) {
    val numberOfScenarios: Int by numberOfScenariosOption()
    val scenarioLength: Int by scenarioLengthOption()
    val scenariosSeeds: IntRange by scenariosSeedsOption().required()

    // val method: String? by methodOption()
    // val maxGuardSize: Int? by maxGuardSizeOption()
    // val maxPlateauWidth: Int? by maxPlateauWidthOption()
    val outBaseDir: File by outBaseDirOption()
}

private class BatchRandomExperimentSolverOptions : OptionGroup(BATCH_SOLVER_OPTIONS) {
    val solver: String by solverOption().required()
    val solverSeeds: IntRange by solverSeedsOption().required()
}

private class BatchRandomExperimentOptions : OptionGroup(BATCH_OPTIONS) {
    val fbsatBin: String by fbsatBinOption()
    val sbatchBin: String by sbatchBinOption()
    val sbatchOptions: List<String> by sbatchOptionsOption()
    val isIgnoreLock: Boolean by isIgnoreLockOption()
    val isDryRun: Boolean by isDryRunOption()
}

class BatchRandomExperimentCommand : CliktCommand(name = "randexp-batch") {
    private val dataOptions by BatchRandomExperimentDataOptions()
    private val inferenceOptions by BatchRandomExperimentInferenceOptions()
    private val solverOptions by BatchRandomExperimentSolverOptions()
    private val batchOptions by BatchRandomExperimentOptions()

    private val isDebug: Boolean by isDebugOption()

    @Suppress("LocalVariableName")
    override fun run() {
        Globals.IS_DEBUG = isDebug

        logger.debug { "Running $commandName..." }

        // Data options
        val C = dataOptions.numberOfStates
        // val Pgen = dataOptions.maxGuardSize
        val X = dataOptions.numberOfInputVariables
        val Z = dataOptions.numberOfOutputVariables

        // Inference options
        val n = inferenceOptions.numberOfScenarios
        val k = inferenceOptions.scenarioLength
        // val method = inferenceOptions.method
        // val P = inferenceOptions.maxGuardSize
        // val w = inferenceOptions.maxPlateauWidth
        val outBaseDir = inferenceOptions.outBaseDir
        val solverName = solverOptions.solver

        // Batch options
        val fbsatBin = batchOptions.fbsatBin
        val sbatchBin = batchOptions.sbatchBin
        val sbatchOptions = batchOptions.sbatchOptions
        val isIgnoreLock = batchOptions.isIgnoreLock
        val isDryRun = batchOptions.isDryRun

        // Seeds
        val automatonSeeds = dataOptions.automatonSeeds
        val scenariosSeeds = inferenceOptions.scenariosSeeds
        val solverSeeds = solverOptions.solverSeeds

        // TODO: log all options

        outBaseDir.mkdirs()

        for (automatonSeed in automatonSeeds)
            for (scenariosSeed in scenariosSeeds)
                for (solverSeed in solverSeeds)
                    runBatch(
                        C = C, X = X, Z = Z,
                        n = n, k = k,
                        solver = solverName,
                        automatonSeed = automatonSeed,
                        scenariosSeed = scenariosSeed,
                        solverSeed = solverSeed,
                        outBaseDir = outBaseDir,
                        fbsatBin = fbsatBin,
                        sbatchBin = sbatchBin,
                        sbatchOptions = sbatchOptions,
                        ignoreLock = isIgnoreLock,
                        dryRun = isDryRun,
                    )
    }
}
