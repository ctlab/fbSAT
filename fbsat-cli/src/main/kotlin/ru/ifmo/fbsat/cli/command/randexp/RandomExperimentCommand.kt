@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.cli.command.randexp

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.ParameterHolder
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.convert
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.defaultLazy
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.options.validate
import com.github.ajalt.clikt.parameters.types.choice
import com.github.ajalt.clikt.parameters.types.double
import com.github.ajalt.clikt.parameters.types.int
import com.mongodb.client.MongoDatabase
import org.litote.kmongo.KMongo
import org.litote.kmongo.serialization.registerModule
import ru.ifmo.fbsat.cli.command.infer.options.EXTRA_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.cli.command.infer.options.isDebugOption
import ru.ifmo.fbsat.cli.command.infer.options.isRenderWithDotOption
import ru.ifmo.fbsat.cli.command.infer.options.maxGuardSizeOption
import ru.ifmo.fbsat.cli.command.infer.options.maxPlateauWidthOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfStatesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.serializers.fbsatSerializersModule
import java.io.File

private val logger = MyLogger {}

internal const val DATA_OPTIONS = "Data Options"
internal const val INFERENCE_OPTIONS = "Inference Options"
internal const val DB_OPTIONS = "DB Options"

private fun ParameterHolder.numberOfInputEventsOption() =
    option(
        "-I",
        help = "Number of input events",
        metavar = "<int>"
    ).int()

private fun ParameterHolder.numberOfOutputEventsOption() =
    option(
        "-O",
        help = "Number of output events",
        metavar = "<int>"
    ).int()

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

private fun ParameterHolder.automatonSeedOption() =
    option(
        "--automaton-seed",
        // "--seed-automaton",
        help = "Seed for automaton",
        metavar = "<int>"
    ).int()

private fun ParameterHolder.validationScenariosSeedOption() =
    option(
        "--validation-scenarios-seed",
        // "--seed-validation",
        help = "Seed for validation scenarios",
        metavar = "<int>"
    ).int()

private fun ParameterHolder.numberOfScenariosOption() =
    option(
        "-n",
        help = "Number of random scenarios to generate",
        metavar = "<int>"
    ).int()

private fun ParameterHolder.scenarioLengthOption() =
    option(
        "-k",
        help = "Length of each scenario",
        metavar = "<int>"
    ).int()

private fun ParameterHolder.scenariosSeedOption() =
    option(
        "--scenarios-seed",
        // "--seed-scenarios",
        help = "Seed for scenarios",
        metavar = "<int>"
    ).int()

private fun ParameterHolder.methodOption() =
    option(
        "-m", "--method",
        help = "Method of inference",
    ).choice(
        "basic-min",
        "extended-min",
        "extended-min-ub",
    )

private fun ParameterHolder.isOverwriteOption() =
    option(
        "--overwrite",
        help = "Ignore existing results"
    ).flag()

private fun ParameterHolder.isSaveToDbOption() =
    option(
        "--save-to-db",
        help = "Save results to database (MongoDB)"
    ).flag()

private fun ParameterHolder.connectionStringOption() =
    option(
        "--db-addr",
        help = "MongoDB connection string"
    ).default("mongodb://localhost")

private fun ParameterHolder.databaseNameOption() =
    option(
        "--db-name",
        help = "MongoDB database name"
    ).default("test")

private class RandomExperimentDataOptions : OptionGroup(DATA_OPTIONS) {
    val numberOfStates: Int by numberOfStatesOption().required()
    val numberOfInputEvents: Int by numberOfInputEventsOption().default(1)
    val numberOfOutputEvents: Int by numberOfOutputEventsOption().default(1)
    val numberOfInputVariables: Int by numberOfInputVariablesOption().required()
    val numberOfOutputVariables: Int by numberOfOutputVariablesOption().required()
    val maxGuardSize: Int by maxGuardSizeGenOption().default(5)
    val automatonSeed: Int by automatonSeedOption().required()
    val validationScenariosSeed: Int by validationScenariosSeedOption()
        .defaultLazy(defaultForHelp = "automatonSeed+10000") { automatonSeed + 10000 }
}

private class RandomExperimentInferenceOptions : OptionGroup(INFERENCE_OPTIONS) {
    val numberOfScenarios: Int by numberOfScenariosOption().default(0).validate {
        if (method != null) {
            require(it > 0) { "must be > 0" }
        } else {
            require(it >= 0) { "must be non-negative" }
        }
    }
    val scenarioLength: Int by scenarioLengthOption().default(0).validate {
        if (method != null) {
            require(it > 0) { "must be > 0" }
        } else {
            require(it >= 0) { "must be non-negative" }
        }
    }
    val scenariosSeed: Int by scenariosSeedOption().required()
    val method: String? by methodOption()
    val maxGuardSize: Int? by maxGuardSizeOption()
    val maxPlateauWidth: Int? by maxPlateauWidthOption()
}

private class RandomExperimentExtraOptions : OptionGroup(EXTRA_OPTIONS) {
    val isDebug: Boolean by isDebugOption()
    val isRenderWithDot: Boolean by isRenderWithDotOption()
    val isOverwrite: Boolean by isOverwriteOption()
    val outDir: File by outDirOption()

    // Note: timeout is in milliseconds, but in CLI we expect seconds.
    val timeout: Long? by option(
        "-t", "--timeout",
        help = "Timeout",
        metavar = "<sec>"
    ).double().convert { (it * 1000).toLong() }
}

private class RandomExperimentDbOptions : OptionGroup(DB_OPTIONS) {
    val isSaveToDb: Boolean by isSaveToDbOption()
    val connectionString: String by connectionStringOption()
    val databaseName: String by databaseNameOption()
}

// Example usage:
//  fbsat randexp -C 4 -X 3 -Z 2 --automaton-seed 1 --method extended-min -P 5 -n 10 -k 50 --scenarios-seed 1 --glucose --solver-seed 1 --solver-rnd-freq 0.1 --solver-rnd-pol --solver-rnd-init --outdir out/randexp --debug

class RandomExperimentCommand : CliktCommand(name = "randexp") {
    private val dataOptions by RandomExperimentDataOptions()
    private val inferenceOptions by RandomExperimentInferenceOptions()
    private val solverOptions by SolverOptions()
    private val dbOptions by RandomExperimentDbOptions()
    private val extraOptions by RandomExperimentExtraOptions()

    @Suppress("LocalVariableName")
    override fun run() {
        Globals.IS_DEBUG = extraOptions.isDebug
        Globals.IS_RENDER_WITH_DOT = extraOptions.isRenderWithDot

        logger.debug { "Running $commandName..." }

        // Extra options
        val isOverwrite = extraOptions.isOverwrite
        val timeout = extraOptions.timeout
        val outDir = extraOptions.outDir

        val resultFile = outDir.resolve("result.json")
        val needRun = when {
            resultFile.exists() -> {
                if (isOverwrite) {
                    logger.info { "'$resultFile' already exists, but we overwrite it." }
                    true
                } else {
                    logger.info { "'$resultFile' already exists => not running. Use '--overwrite' flag to force overwriting existing results." }
                    false
                }
            }
            else -> true
        }

        if (needRun) {
            // Data options
            val C = dataOptions.numberOfStates
            val Pgen = dataOptions.maxGuardSize
            val I = dataOptions.numberOfInputEvents
            val O = dataOptions.numberOfOutputEvents
            val X = dataOptions.numberOfInputVariables
            val Z = dataOptions.numberOfOutputVariables
            val n = inferenceOptions.numberOfScenarios
            val k = inferenceOptions.scenarioLength

            // Inference options
            val method = inferenceOptions.method
            val P = inferenceOptions.maxGuardSize
            val w = inferenceOptions.maxPlateauWidth

            // Seeds
            val automatonSeed = dataOptions.automatonSeed
            val validationScenariosSeed = dataOptions.validationScenariosSeed
            val scenariosSeed = inferenceOptions.scenariosSeed
            val solverSeed = solverOptions.solverSeed

            // DB options
            val isSaveToDb = dbOptions.isSaveToDb
            val connectionString = dbOptions.connectionString
            val databaseName = dbOptions.databaseName

            // TODO: log all options

            lateinit var database: MongoDatabase
            if (isSaveToDb) {
                logger.debug { "Connecting to MongoDB on '$connectionString'..." }
                val client = KMongo.createClient(connectionString)
                database = client.getDatabase(databaseName)

                registerModule(fbsatSerializersModule)
            }

            val data = ExperimentData.generate(
                automatonParams = ExperimentData.AutomatonParams(
                    C = C, P = Pgen, I = I, O = O, X = X, Z = Z,
                    seed = automatonSeed
                ),
                validationScenariosParams = ExperimentData.ValidationScenariosParams(
                    n = 100,
                    k = 100,
                    seed = validationScenariosSeed
                ),
                scenariosParams = ExperimentData.ScenariosParams(
                    n = n,
                    k = k,
                    seed = scenariosSeed
                )
            )
            val dataFile = outDir.resolve("data.json")
            data.saveTo(dataFile)
            if (isSaveToDb) {
                data.saveTo(database)
            }
            data.automaton.dump(outDir, name = "generated-automaton")

            if (method != null) {
                val inferenceMethod = when (method) {
                    "basic-min" -> {
                        InferenceMethod.BasicMin
                    }
                    "extended-min" -> {
                        requireNotNull(P) { "Parameter P must be specified" }
                        InferenceMethod.ExtendedMin(P)
                    }
                    "extended-min-ub" -> {
                        InferenceMethod.ExtendedMinUB(w)
                    }
                    else -> error("Method '$method' is not supported")
                }
                val solverInfo = SolverInfo(
                    name = solverOptions.solverBackend.name.lowercase(),
                    seed = solverSeed ?: 0
                )
                val solver = solverOptions.solver
                val inferenceParams = InferenceParams(
                    method = inferenceMethod,
                    solverInfo = solverInfo,
                    outDir = outDir
                )

                val result = runExperiment(
                    data = data,
                    inferenceParams = inferenceParams,
                    solverInit = { solver },
                    timeout = timeout,
                    outDir = outDir
                )
                result.saveTo(resultFile)
                if (isSaveToDb) {
                    result.saveTo(database)
                }
            }
        }
    }
}
