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
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.choice
import com.github.ajalt.clikt.parameters.types.double
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import com.mongodb.client.MongoDatabase
import org.litote.kmongo.KMongo
import org.litote.kmongo.serialization.registerModule
import ru.ifmo.fbsat.cli.command.infer.options.EXTRA_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SOLVER_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.isDebugOption
import ru.ifmo.fbsat.cli.command.infer.options.isRenderWithDotOption
import ru.ifmo.fbsat.cli.command.infer.options.maxGuardSizeOption
import ru.ifmo.fbsat.cli.command.infer.options.maxPlateauWidthOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfStatesOption
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.serializers.fbsatSerializersModule
import java.io.File

private val logger = MyLogger {}

private fun parseRange(s: String): IntRange =
    if (s.contains("-")) {
        val (min, max) = s.split("-", limit = 2).map(String::toInt)
        min..max
    } else {
        val x = s.toInt()
        x..x
    }

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

private fun ParameterHolder.scenarioLengthOption() =
    option(
        "-k",
        help = "Length of each scenario",
        metavar = "<int>"
    ).int()

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

private fun ParameterHolder.solverNameOption() =
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

private class MultiRandomExperimentDataOptions : OptionGroup(DATA_OPTIONS) {
    val numberOfStates: Int by numberOfStatesOption().required()
    val numberOfInputEvents: Int by numberOfInputEventsOption().default(1)
    val numberOfOutputEvents: Int by numberOfOutputEventsOption().default(1)
    val numberOfInputVariables: Int by numberOfInputVariablesOption().required()
    val numberOfOutputVariables: Int by numberOfOutputVariablesOption().required()
    val maxGuardSize: Int by maxGuardSizeGenOption().default(5)
    val automatonSeeds: IntRange by automatonSeedsOption().required()
}

private class MultiRandomExperimentInferenceOptions : OptionGroup(INFERENCE_OPTIONS) {
    val numberOfScenarios: Int by numberOfScenariosOption()
        .required()
        .check("value must > 0") { it > 0 }
    val scenarioLength: Int by scenarioLengthOption()
        .required()
        .check("value must > 0") { it > 0 }
    val scenariosSeeds: IntRange by scenariosSeedsOption().required()
    val method: String? by methodOption()
    val maxGuardSize: Int? by maxGuardSizeOption()
    val maxPlateauWidth: Int? by maxPlateauWidthOption()
}

private class MultiRandomExperimentSolverOptions : OptionGroup(SOLVER_OPTIONS) {
    val solverName: String by solverNameOption().required()
    val solverSeeds: IntRange by solverSeedsOption().required()
}

private class MultiRandomExperimentExtraOptions : OptionGroup(EXTRA_OPTIONS) {
    val outBaseDir: File by outBaseDirOption()
    val isDebug: Boolean by isDebugOption()
    val isOverwrite: Boolean by isOverwriteOption()
    val isRenderWithDot: Boolean by isRenderWithDotOption()

    // Note: timeout is in milliseconds, but in CLI we expect seconds.
    val timeout: Long? by option(
        "-t", "--timeout",
        help = "Timeout",
        metavar = "<sec>"
    ).double().convert { (it * 1000).toLong() }
}

private class MultiRandomExperimentDbOptions : OptionGroup(DB_OPTIONS) {
    val isSaveToDb: Boolean by isSaveToDbOption()
    val connectionString: String by connectionStringOption()
    val databaseName: String by databaseNameOption()
}

class MultiRandomExperimentCommand : CliktCommand(name = "randexp-multi") {
    private val dataOptions by MultiRandomExperimentDataOptions()
    private val inferenceOptions by MultiRandomExperimentInferenceOptions()
    private val solverOptions by MultiRandomExperimentSolverOptions()
    private val dbOptions by MultiRandomExperimentDbOptions()
    private val extraOptions by MultiRandomExperimentExtraOptions()

    @Suppress("LocalVariableName")
    override fun run() {
        Globals.IS_DEBUG = extraOptions.isDebug
        Globals.IS_RENDER_WITH_DOT = extraOptions.isRenderWithDot

        logger.debug { "Running $commandName..." }

        // Data options
        val C = dataOptions.numberOfStates
        val Pgen = dataOptions.maxGuardSize
        val I = dataOptions.numberOfInputEvents
        val O = dataOptions.numberOfOutputEvents
        val X = dataOptions.numberOfInputVariables
        val Z = dataOptions.numberOfOutputVariables

        // Inference options
        val n = inferenceOptions.numberOfScenarios
        val k = inferenceOptions.scenarioLength
        val method = inferenceOptions.method
        val P = inferenceOptions.maxGuardSize
        val w = inferenceOptions.maxPlateauWidth
        val solverName = solverOptions.solverName

        // Seeds
        val automatonSeeds = dataOptions.automatonSeeds
        val scenariosSeeds = inferenceOptions.scenariosSeeds
        val solverSeeds = solverOptions.solverSeeds

        // DB options
        val isSaveToDb = dbOptions.isSaveToDb
        val connectionString = dbOptions.connectionString
        val databaseName = dbOptions.databaseName

        // Extra options
        val outBaseDir = extraOptions.outBaseDir
        val isOverwrite = extraOptions.isOverwrite
        val timeout = extraOptions.timeout

        // TODO: log all options

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

        lateinit var database: MongoDatabase
        if (isSaveToDb) {
            logger.debug { "Connecting to MongoDB on '$connectionString'..." }
            val client = KMongo.createClient(connectionString)
            database = client.getDatabase(databaseName)

            registerModule(fbsatSerializersModule)
        }

        // dataCache :: (automatonSeed, scenarioSeed) -> data
        val dataCache: MutableMap<Pair<Int, Int>, ExperimentData> = mutableMapOf()

        logger.info { "Running for as={$automatonSeeds}, ss={$scenariosSeeds}, rs={$solverSeeds}" }
        for (automatonSeed in automatonSeeds) {
            val automatonParams = ExperimentData.AutomatonParams(
                C = C, P = Pgen, I = I, O = O, X = X, Z = Z,
                seed = automatonSeed
            )
            val validationScenariosParams = ExperimentData.ValidationScenariosParams(
                n = 100,
                k = 100,
                seed = automatonSeed + 10000
            )

            for (scenariosSeed in scenariosSeeds) {
                val scenariosParams = ExperimentData.ScenariosParams(
                    n = n,
                    k = k,
                    seed = scenariosSeed
                )

                for (solverSeed in solverSeeds) {
                    val expName = "exp" +
                        "_C${C}" +
                        "_P${Pgen}" +
                        "_I${I}" +
                        "_O${O}" +
                        "_X${X}" +
                        "_Z${Z}" +
                        "_n${n}" +
                        "_k${k}" +
                        "_${inferenceMethod.toNameString()}" +
                        "_$solverName" +
                        "_a${automatonSeed}" +
                        "_s${scenariosSeed}" +
                        "_r${solverSeed}"
                    val outDir = outBaseDir.resolve(expName)
                    val resultFile = outDir.resolve("result.json")
                    val needRun = when {
                        resultFile.exists() -> {
                            if (isOverwrite) {
                                logger.info { "'$resultFile' already exists, but we ignore it due to '--overwrite' flag." }
                                true
                            } else {
                                logger.info { "'$resultFile' already exists => not running. Use '--overwrite' flag to ignore existing results." }
                                false
                            }
                        }
                        else -> true
                    }

                    if (needRun) {
                        logger.info { "Running for a = $automatonSeed, s = $scenariosSeed, r = $solverSeed..." }

                        val data = dataCache.computeIfAbsent(Pair(automatonSeed, scenariosSeed)) {
                            ExperimentData.generate(
                                automatonParams = automatonParams,
                                validationScenariosParams = validationScenariosParams,
                                scenariosParams = scenariosParams
                            )
                        }
                        val dataFile = outDir.resolve("data.json")
                        data.saveTo(dataFile)
                        if (isSaveToDb) {
                            data.saveTo(database)
                        }
                        data.automaton.dump(outDir, name = "generated-automaton")

                        val solverInfo = SolverInfo(
                            name = solverName,
                            seed = solverSeed
                        )
                        val inferenceParams = InferenceParams(
                            method = inferenceMethod,
                            solverInfo = solverInfo,
                            outDir = outDir
                        )

                        val result = runExperiment(
                            data = data,
                            inferenceParams = inferenceParams,
                            timeout = timeout,
                            outDir = outDir
                        )
                        result.saveTo(resultFile)
                        if (isSaveToDb) {
                            result.saveTo(database)
                        }
                    } else {
                        logger.info { "Skipping a = $automatonSeed, s = $scenariosSeed, r = $solverSeed" }
                    }
                }
            }
        }
    }
}
