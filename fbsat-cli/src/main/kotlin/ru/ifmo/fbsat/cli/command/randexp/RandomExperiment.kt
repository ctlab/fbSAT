@file:Suppress("PublicApiImplicitType", "DuplicatedCode")

package ru.ifmo.fbsat.cli.command.randexp

import com.github.lipen.satlib.solver.CadicalSolver
import com.github.lipen.satlib.solver.GlucoseSolver
import com.github.lipen.satlib.solver.MiniSatSolver
import com.github.lipen.satlib.solver.Solver
import com.mongodb.client.MongoDatabase
import com.mongodb.client.model.CountOptions
import com.soywiz.klock.PerformanceCounter
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import okio.buffer
import okio.sink
import org.litote.kmongo.KMongo
import org.litote.kmongo.eq
import org.litote.kmongo.getCollection
import org.litote.kmongo.replaceUpsert
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.generateRandomScenario
import ru.ifmo.fbsat.core.automaton.newRandomAutomaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.single.basic.basicMin
import ru.ifmo.fbsat.core.task.single.extended.extendedMin
import ru.ifmo.fbsat.core.task.single.extended.extendedMinUB
import ru.ifmo.fbsat.core.utils.EpsilonOutputEvents
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.StartStateAlgorithms
import ru.ifmo.fbsat.core.utils.ensureParentExists
import ru.ifmo.fbsat.core.utils.scope
import ru.ifmo.fbsat.core.utils.serializers.FileAsStringSerializer
import ru.ifmo.fbsat.core.utils.serializers.fbsatSerializersModule
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.withIndex
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File
import kotlin.math.roundToInt
import kotlin.random.Random

private val logger = MyLogger {}

private val myJson = Json {
    serializersModule = fbsatSerializersModule
    prettyPrint = true
}

@Serializable
data class ExperimentData(
    val name: String,
    val params: DataParams,
    val automaton: Automaton, // generated
    val validationScenarios: List<PositiveScenario>,
    val scenarios: List<PositiveScenario>,
) {
    @Serializable
    data class DataParams(
        val automatonParams: AutomatonParams,
        val validationScenariosParams: ValidationScenariosParams,
        val scenariosParams: ScenariosParams,
    )

    @Serializable
    data class AutomatonParams(
        val C: Int,
        val P: Int,
        val I: Int,
        val O: Int,
        val X: Int,
        val Z: Int,
        val seed: Int,
    )

    @Serializable
    data class ValidationScenariosParams(
        val n: Int,
        val k: Int,
        val seed: Int,
    )

    @Serializable
    data class ScenariosParams(
        val n: Int,
        val k: Int,
        val seed: Int,
    )

    @OptIn(ExperimentalSerializationApi::class)
    fun saveTo(file: File) {
        logger.debug { "Saving experiment data to '$file'..." }
        file.ensureParentExists().sink().buffer().use {
            it.writeln(myJson.encodeToString(this))
        }
    }

    fun saveTo(database: MongoDatabase) {
        val col = database.getCollection<ExperimentData>()
        if (col.countDocuments(::name eq name, CountOptions().limit(1)) > 0) {
            logger.debug { "Experiment data already exists in MongoDB collection '${col.namespace}'" }
            // Note: we do not replace existing data
        } else {
            logger.debug { "Saving experiment data to MongoDB collection '${col.namespace}'..." }
            col.insertOne(this)
        }
    }

    fun saveToDb(connectionString: String, databaseName: String = "test") {
        logger.debug { "Connecting to MongoDB on '$connectionString'..." }
        val client = KMongo.createClient(connectionString)
        val database = client.getDatabase(databaseName)
        saveTo(database)
    }

    companion object {
        fun generate(
            automatonParams: AutomatonParams,
            validationScenariosParams: ValidationScenariosParams,
            scenariosParams: ScenariosParams,
        ): ExperimentData {
            val name = getDataName(automatonParams, scenariosParams)
            logger.info("Generating '$name'...")

            logger.info { "Generating random automaton with ${automatonParams}..." }
            val randomAutomaton = newRandomAutomaton(automatonParams)
            logger.info { "Generated random automaton:" }
            randomAutomaton.pprint()

            val nVal = validationScenariosParams.n
            val kVal = validationScenariosParams.k
            val validationScenariosSeed = validationScenariosParams.seed

            logger.info { "Generating $nVal validation scenarios (each of length $kVal) with seed=$validationScenariosSeed..." }
            val validationScenariosRandom = Random(validationScenariosSeed)
            val validationScenarios = List(nVal) {
                randomAutomaton.generateRandomScenario(kVal, random = validationScenariosRandom)
            }

            val n = scenariosParams.n
            val k = scenariosParams.k
            val scenariosSeed = scenariosParams.seed

            logger.info { "Generating $n random scenario (each of length $k) with seed=$scenariosSeed..." }
            val scenariosRandom = Random(scenariosSeed)
            val scenarios = List(n) {
                randomAutomaton.generateRandomScenario(k, random = scenariosRandom)
            }

            return ExperimentData(
                name = name,
                params = DataParams(
                    automatonParams = automatonParams,
                    validationScenariosParams = validationScenariosParams,
                    scenariosParams = scenariosParams,
                ),
                automaton = randomAutomaton,
                validationScenarios = validationScenarios,
                scenarios = scenarios,
            )
        }
    }
}

private fun newRandomAutomaton(
    automatonParams: ExperimentData.AutomatonParams,
): Automaton = with(automatonParams) {
    newRandomAutomaton(
        C = C, P = P, I = I, O = O, X = X, Z = Z,
        random = Random(seed)
    )
}

private fun getDataName(
    automatonParams: ExperimentData.AutomatonParams,
    scenariosParams: ExperimentData.ScenariosParams,
): String = "data" +
    "_C${automatonParams.C}" +
    "_P${automatonParams.P}" +
    "_I${automatonParams.I}" +
    "_O${automatonParams.O}" +
    "_X${automatonParams.X}" +
    "_Z${automatonParams.Z}" +
    "_a${automatonParams.seed}" +
    "_n${scenariosParams.n}" +
    "_k${scenariosParams.k}" +
    "_s${scenariosParams.seed}"

@Serializable
data class InferenceParams(
    val method: InferenceMethod,
    @SerialName("solver")
    val solverInfo: SolverInfo,
    @Serializable(with = FileAsStringSerializer::class)
    val outDir: File,
)

@Serializable
sealed class InferenceMethod {
    abstract fun toNameString(): String

    @Serializable
    @SerialName("BasicMin")
    object BasicMin : InferenceMethod() {
        override fun toNameString(): String {
            return "basic-min"
        }
    }

    @Serializable
    @SerialName("ExtendedMin")
    data class ExtendedMin(val P: Int) : InferenceMethod() {
        override fun toNameString(): String {
            return "extended-min_P$P"
        }
    }

    @Serializable
    @SerialName("ExtendedMinUB")
    data class ExtendedMinUB(val w: Int?) : InferenceMethod() {
        override fun toNameString(): String {
            return "extended-min-UB_w${if (w == null) "Inf" else "$w"}"
        }
    }
}

@Serializable
data class SolverInfo(
    val name: String,
    val seed: Int, // just pass 0 if solver does not support seed
) {
    fun toNameString(): String {
        return "${name}_seed${seed}"
    }
}

fun SolverInfo.instantiate(): Solver {
    logger.info { "Creating solver '$name' with seed=$seed..." }
    // MiniSat/Glucose options to enable randomization (to some extent):
    val initialRandomVarFreq = 0.01
    val initialRandomPolarities = true
    val initialRandomInitialActivities = true
    return when (name) {
        "minisat" -> {
            logger.debug { "Using initialRandomVarFreq = $initialRandomVarFreq" }
            logger.debug { "Using initialRandomPolarities = $initialRandomPolarities" }
            logger.debug { "Using initialRandomInitialActivities = $initialRandomInitialActivities" }
            MiniSatSolver(
                initialSeed = seed.toDouble(),
                initialRandomVarFreq = initialRandomVarFreq,
                initialRandomPolarities = initialRandomPolarities,
                initialRandomInitialActivities = initialRandomInitialActivities
            )
        }
        "glucose" -> {
            logger.debug { "Using initialRandomVarFreq = $initialRandomVarFreq" }
            logger.debug { "Using initialRandomPolarities = $initialRandomPolarities" }
            logger.debug { "Using initialRandomInitialActivities = $initialRandomInitialActivities" }
            GlucoseSolver(
                initialSeed = seed.toDouble(),
                initialRandomVarFreq = initialRandomVarFreq,
                initialRandomPolarities = initialRandomPolarities,
                initialRandomInitialActivities = initialRandomInitialActivities
            )
        }
        "cadical" -> {
            CadicalSolver(initialSeed = seed)
        }
        else -> error("Instantiation of '$name' is not implemented here. In order to use arbitrary solver, create it manually, and pass it inside `solverInit` lambda to the `runExperiment` function.")
    }
}

@Serializable
data class ExperimentResult(
    val name: String,
    val dataName: String,
    val dataParams: ExperimentData.DataParams,
    val inferenceParams: InferenceParams,
    val inferenceResult: InferenceResult,
) {
    @Serializable
    data class InferenceResult(
        val status: Status,
        val automaton: Automaton?, // inferred
        val forwardCheck: Int, // 0 if automaton is null
        val time: Double,
        val solverStats: Map<String, Long>,
    ) {
        @Serializable
        enum class Status {
            SAT, UNSAT, TIMEOUT
        }
    }

    @OptIn(ExperimentalSerializationApi::class)
    fun saveTo(file: File) {
        logger.debug { "Saving experiment result to '$file'..." }
        file.ensureParentExists().sink().buffer().use {
            it.writeln(myJson.encodeToString(this))
        }
    }

    fun saveTo(database: MongoDatabase) {
        val col = database.getCollection<ExperimentResult>()
        logger.debug { "Saving experiment result to MongoDB collection '${col.namespace}'..." }
        // Note: existing result have to be replaced
        col.replaceOne(::name eq name, this, replaceUpsert())
    }

    fun saveToDb(connectionString: String, databaseName: String = "test") {
        logger.debug { "Connecting to MongoDB on '$connectionString'..." }
        val client = KMongo.createClient(connectionString)
        val database = client.getDatabase(databaseName)
        saveTo(database)
    }
}

private fun getExperimentName(
    data: ExperimentData,
    params: InferenceParams,
): String {
    return "exp" +
        "_${params.method.toNameString()}" +
        "_${params.solverInfo.toNameString()}" +
        "__${data.name}"
}

@Suppress("LocalVariableName")
fun runExperiment(
    data: ExperimentData,
    inferenceParams: InferenceParams,
    name: String = getExperimentName(data, inferenceParams),
    // Note: if you want to run the experiment with an existing solver,
    //  pass some representative SolverInfo inside `inferenceParams`,
    //  and also pass the existing `solver` via lambda (e.g. `solverInit = { solver }`)
    solverInit: (SolverInfo) -> Solver = { it.instantiate() },
    timeout: Long? = null, // milliseconds
    outDir: File = inferenceParams.outDir,
    autoClose: Boolean = false,
): ExperimentResult {
    logger.info { "Running experiment '$name'..." }

    // TODO: log params

    logger.info { "Building scenario tree..." }
    val tree = PositiveScenarioTree.fromScenarios(
        scenarios = data.scenarios,
        inputNames = data.automaton.inputNames,
        outputNames = data.automaton.outputNames,
        inputEvents = data.automaton.inputEvents,
        outputEvents = data.automaton.outputEvents
    )
    logger.info { "Tree size: ${tree.size}" }

    val solver = solverInit(inferenceParams.solverInfo)
    val inferrer = Inferrer(solver, inferenceParams.outDir, timeout = timeout)

    // Globals.START_STATE_ALGORITHMS = StartStateAlgorithms.ZERONOTHING
    Globals.START_STATE_ALGORITHMS = StartStateAlgorithms.ANY
    Globals.EPSILON_OUTPUT_EVENTS = EpsilonOutputEvents.NONE
    Globals.IS_FORBID_TRANSITIONS_TO_FIRST_STATE = false

    val method = inferenceParams.method

    logger.info { "Performing synthesis of an automaton using $method..." }
    val timeInferStart = PerformanceCounter.reference
    val inferredAutomaton = when (method) {
        is InferenceMethod.BasicMin -> {
            inferrer.basicMin(tree)
        }
        is InferenceMethod.ExtendedMin -> {
            inferrer.extendedMin(tree, maxGuardSize = method.P)
        }
        is InferenceMethod.ExtendedMinUB -> {
            inferrer.extendedMinUB(tree, maxPlateauWidth = method.w)
        }
    }
    val timeInfer = timeSince(timeInferStart)

    val solverStats: MutableMap<String, Long> = mutableMapOf()
    when (solver) {
        is MiniSatSolver -> {
            solverStats["propagations"] = solver.backend.numberOfPropagations
            solverStats["conflicts"] = solver.backend.numberOfConflicts
            solverStats["decisions"] = solver.backend.numberOfDecisions
        }
        is GlucoseSolver -> {
            solverStats["propagations"] = solver.backend.numberOfPropagations
            solverStats["conflicts"] = solver.backend.numberOfConflicts
            solverStats["decisions"] = solver.backend.numberOfDecisions
        }
        is CadicalSolver -> {
            solverStats["conflicts"] = solver.backend.numberOfConflicts
            solverStats["decisions"] = solver.backend.numberOfDecisions
            solverStats["restarts"] = solver.backend.numberOfRestarts
            solverStats["propagations"] = solver.backend.numberOfPropagations
        }
        else -> {
            logger.debug { "$solver does not support querying statistics" }
        }
    }

    // Close the solver to avoid memory leaks
    if (autoClose) {
        inferrer.solver.close()
    }

    var forwardCheck = 0

    if (inferredAutomaton != null) {
        // logger.info { "Original (random generated) automaton:" }
        // data.automaton.pprint()
        logger.info { "Inferred automaton:" }
        inferredAutomaton.pprint()
        logger.info { "Inference done in %.2f s".format(timeInfer.seconds) }
        inferredAutomaton.dump(outDir, "inferred-automaton")

        logger.info { "Performing forward-check..." }
        val total = data.validationScenarios.size
        var okay = 0
        var failed = 0
        for ((i, scenario) in data.validationScenarios.withIndex(start = 1)) {
            if (inferredAutomaton.verify(scenario)) {
                // log.debug { "Scenario #$i: OK" }
                okay++
            } else {
                // log.error { "Scenario #$i: FAIL" }
                failed++
            }
        }
        val percentOkay = okay.toDouble() / total * 100
        logger.info { "Forward-check: $okay of $total (${percentOkay.roundToInt()}%)" }
        forwardCheck = okay
    } else {
        logger.error { "Could not infer an automaton after %.2f s".format(timeInfer.seconds) }
    }

    val status = when {
        inferrer.isTimeout() -> ExperimentResult.InferenceResult.Status.TIMEOUT
        inferredAutomaton == null -> ExperimentResult.InferenceResult.Status.UNSAT
        else -> ExperimentResult.InferenceResult.Status.SAT
    }

    return ExperimentResult(
        name = name,
        dataName = data.name,
        dataParams = data.params,
        inferenceParams = inferenceParams,
        inferenceResult = ExperimentResult.InferenceResult(
            status = status,
            automaton = inferredAutomaton,
            forwardCheck = forwardCheck,
            time = timeInfer.seconds,
            solverStats = solverStats
        )
    )
}

@OptIn(ExperimentalSerializationApi::class)
@Suppress("LocalVariableName")
fun main() {
    val timeStart = PerformanceCounter.reference

    Globals.IS_DEBUG = true

    scope {
        // val C = 8 // number of states
        // val P = 5 // max guard size
        // val I = 1 // number of input events
        // val O = 1 // number of output events
        // val X = 5 // number of input variables
        // val Z = 7 // number of output variables
        val C = 5 // number of states
        val P = 5 // max guard size
        val I = 1 // number of input events
        val O = 1 // number of output events
        val X = 3 // number of input variables
        val Z = 2 // number of output variables

        // Sets:
        //  10x50
        val n = 10 // number of scenarios
        val k = 50 // scenario length

        // val solverName = "minisat"
        val solverName = "glucose"

        // val method = InferenceMethod.BasicMin
        val method = InferenceMethod.ExtendedMin(P = P)

        // val outDir = File("out/${DateTime.nowLocal().format("yyyy-MM-dd_HH-mm-ss")}")
        val outDir = File("out/randexp/t")

        val automatonSeed = 1
        val scenariosSeed = 1
        val solverSeed = 1

        val data = ExperimentData.generate(
            automatonParams = ExperimentData.AutomatonParams(
                C = C, P = P, I = I, O = O, X = X, Z = Z,
                seed = automatonSeed
            ),
            validationScenariosParams = ExperimentData.ValidationScenariosParams(
                n = 100,
                k = 100,
                seed = automatonSeed + 10000
            ),
            scenariosParams = ExperimentData.ScenariosParams(
                n = n,
                k = k,
                seed = scenariosSeed
            )
        )

        val dataFile = outDir.resolve("data.json")
        dataFile.ensureParentExists().sink().buffer().use {
            it.writeln(myJson.encodeToString(data))
        }

        val inferenceParams = InferenceParams(
            method = method,
            solverInfo = SolverInfo(
                name = solverName,
                seed = solverSeed
            ),
            outDir = outDir
        )

        val result = runExperiment(data, inferenceParams)

        val resultFile = outDir.resolve("result.json")
        resultFile.ensureParentExists().sink().buffer().use {
            it.writeln(myJson.encodeToString(result))
        }
    }

    logger.info("All done in %.3f s".format(timeSince(timeStart).seconds))
}
