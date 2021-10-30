package ru.ifmo.fbsat.cli.entry.braccio.mutex

import com.github.lipen.satlib.solver.GlucoseSolver
import com.github.lipen.satlib.solver.Solver
import com.soywiz.klock.DateTime
import com.soywiz.klock.PerformanceCounter
import okio.buffer
import okio.sink
import okio.source
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.scenario.preprocessed
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.single.extended.extended
import ru.ifmo.fbsat.core.task.single.extended.extendedMin
import ru.ifmo.fbsat.core.utils.EpsilonOutputEvents
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.StartStateAlgorithms
import ru.ifmo.fbsat.core.utils.all
import ru.ifmo.fbsat.core.utils.ensureParentExists
import ru.ifmo.fbsat.core.utils.lineSequence
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

private val logger = MyLogger {}

private fun String.toBool(): Boolean = when (lowercase()) {
    "0", "false" -> false
    "1", "true" -> true
    else -> error("Bad non-boolean string '$this'")
}

private val elementData: MutableMap<ScenarioElement, Any> = mutableMapOf()

data class LogLine(
    val iter: Int,
    val index: Int,
    val data: Map<String, String>,
) {
    companion object {
        private val regexLine by lazy {
            Regex("""^\[iter\s*#\s*(\d+)\]\s*CONTROLLER_(\d+)\s*(.*)$""")
        }

        fun from(s: String): LogLine {
            val m = regexLine.matchEntire(s)
                ?: error("Bad string 's' doesn't match regex")
            val (iter, index, rest) = m.destructured
            val data = rest.split(" ").associate {
                val (name, value) = it.split("=", limit = 2).map(String::trim)
                name to value
            }.toMutableMap()
            data["is_done_all"] = listOf(
                "is_done_m1",
                "is_done_m2",
                "is_done_m3",
                "is_done_m4",
                "is_done_m5",
                "is_done_m6",
            ).map {
                data.getValue(it).toBool()
            }.all().let { if (it) 1 else 0 }.toString()
            return LogLine(iter.toInt(), index.toInt(), data)
        }
    }
}

private fun readScenarioFromLog(
    file: File,
    controllerIndex: Int,
    inputNames: List<String>,
    outputNames: List<String>,
    outputNamesBoolean: List<String>,
    outputNamesIntWithDomain: List<Pair<String, List<Int>>>,
    preprocess: Boolean = false, // FIXME
): PositiveScenario {
    val interestingLines = file.source().buffer().lineSequence().filter {
        it.startsWith("[iter #")
    }.toList().let {
        // Remove last line (because it is probably corrupted)
        it.subList(0, it.size - 1)
    }
    // println("Interesting log lines:")
    // for (line in interestingLines) {
    //     val logLine = LogLine.from(line)
    //     println("  - $logLine")
    // }
    val logLines = interestingLines.map { LogLine.from(it) }
    val elements = logLines
        .filter { it.index == controllerIndex }
        .map { x ->
            val outputValuesBoolean = outputNamesBoolean.associateWith { name ->
                x.data.getValue(name).toBool()
            }
            val outputValuesInt = outputNamesIntWithDomain.flatMap { (name, domain) ->
                domain.map { v ->
                    Pair("${name}_$v", (x.data.getValue(name).toInt() == v))
                }
            }.toMap()
            val map = outputValuesBoolean + outputValuesInt
            val outputValues = outputNames.map { map.getValue(it) }
            ScenarioElement(
                InputAction(
                    event = InputEvent("REQ"),
                    values = InputValues(inputNames.map { x.data.getValue(it).toBool() })
                ),
                OutputAction(
                    event = OutputEvent("CNF"),
                    values = OutputValues(outputValues)
                )
            ).also {
                elementData[it] = x.data
            }
        }
    return if (preprocess) {
        PositiveScenario(elements.preprocessed)
    } else {
        PositiveScenario(elements)
    }
}

fun main() {
    Globals.IS_DEBUG = true

    val dateTimeFormat = "yyyy-MM-dd HH:mm:ss"
    logger.info("Start time: ${DateTime.nowLocal().format(dateTimeFormat)}")
    val timeStart = PerformanceCounter.reference

    val solver: Solver = GlucoseSolver()
    val outDir = File("out/braccio/mutex")
    val inferrer = Inferrer(solver, outDir)

    // input
    val controllerIndex = 1
    val inputNames = listOf(
        "want_cargo_on_out",
        "is_acquired",
        // "is_done_m1",
        // "is_done_m2",
        // "is_done_m3",
        // "is_done_m4",
        // "is_done_m5",
        // "is_done_m6",
        "is_done_all",
    )
    val outputNamesBoolean = listOf(
        "want_to_release",
        "want_to_acquire",
    )
    val anglesBase = listOf(0, 90, 120)
    val anglesShoulder = listOf(65, 80, 100, 115, 140)
    val anglesElbow = listOf(0, 180)
    val anglesWristVer = listOf(10, 15, 165, 170)
    val anglesWristRot = listOf(0, 90)
    val anglesGripper = listOf(15, 73)
    val outputNamesIntWithDomain = listOf(
        "base" to anglesBase,
        "shoulder" to anglesShoulder,
        "elbow" to anglesElbow,
        "wrist_ver" to anglesWristVer,
        "wrist_rot" to anglesWristRot,
        "gripper" to anglesGripper,
    )
    val outputNamesInt =
        anglesBase.map { "base_$it" } +
            anglesShoulder.map { "shoulder_$it" } +
            anglesElbow.map { "elbow_$it" } +
            anglesWristVer.map { "wrist_ver_$it" } +
            anglesWristRot.map { "wrist_rot_$it" } +
            anglesGripper.map { "gripper_$it" }
    val outputNames = outputNamesBoolean + outputNamesInt
    // [lq 002 55050 08 0550 00 53]
    // [ec 091 68114 01 1167 09 17]
    // [ra bbb sssss ee vvvv rr gg]

    val logDir = "data/braccio/mutex/logs"
    val logFiles = listOf(
        "$logDir/serial_mutex_i2c_button1.log",
        "$logDir/serial_mutex_i2c_button2.log",
        "$logDir/serial_mutex_i2c_button3.log",
        "$logDir/serial_mutex_i2c_button123.log",
        "$logDir/serial_mutex_i2c_2work_1wait_loop.log",
        "$logDir/serial_mutex_i2c_but_1and23_loop.log",
        "$logDir/serial_mutex_i2c_but_1and2and3_loop.log",
        "$logDir/serial_mutex_i2c_but_2and13_loop.log",
    )
    val scenarios = logFiles.map { logFile ->
        readScenarioFromLog(
            file = File(logFile),
            controllerIndex = controllerIndex,
            inputNames = inputNames,
            outputNames = outputNames,
            outputNamesBoolean = outputNamesBoolean,
            outputNamesIntWithDomain = outputNamesIntWithDomain,
            preprocess = true
        )
    }
    // for ((i, scenario) in scenarios.withIndex(start = 1)) {
    //     println("Scenario #$i:")
    //     for (element in scenario.elements) {
    //         println("  - $element")
    //     }
    // }
    val scenarioTree = PositiveScenarioTree.fromScenarios(
        scenarios = scenarios,
        inputNames = inputNames,
        outputNames = outputNames,
        inputEvents = listOf(InputEvent("REQ")),
        outputEvents = listOf(OutputEvent("CNF"))
    )
    scenarioTree.printStats()
    logger.info("Input names: ${scenarioTree.inputNames}")
    logger.info("Output names: ${scenarioTree.outputNames}")

    // Globals.IS_FORBID_TRANSITIONS_TO_FIRST_STATE = false
    // Globals.START_STATE_ALGORITHMS = StartStateAlgorithms.ZERONOTHING
    Globals.START_STATE_ALGORITHMS = StartStateAlgorithms.ZERO
    Globals.EPSILON_OUTPUT_EVENTS = EpsilonOutputEvents.NONE
    Globals.IS_ENCODE_EVENTLESS = true
    Globals.IS_ENCODE_CONST_ALGORITHMS = true

    inferrer.onReset = {
        solver.context["outputNamesIntWithDomain"] = outputNamesIntWithDomain
        solver.context["elementData"] = elementData
    }

    // infer
    // val automaton: Automaton? = inferrer.basic(
    //     scenarioTree,
    //     numberOfStates = 13,
    //     maxOutgoingTransitions = 3,
    //     maxTransitions = null,
    // )
    // val automaton: Automaton? = inferrer.basicMin(
    //     scenarioTree,
    //     start = 13,
    // )
    // val automaton: Automaton? = inferrer.extended(
    //     scenarioTree,
    //     numberOfStates = 13, // 13 is SAT
    //     maxOutgoingTransitions = 3,
    //     maxGuardSize = 5
    // )
    val automaton: Automaton? = inferrer.extendedMin(
        scenarioTree,
        // numberOfStates = 13, // 13 is SAT
        maxGuardSize = 5
    )

    if (automaton != null) {
        logger.info("Inferred automaton:")
        automaton.pprint()
        logger.info("Inferred automaton has:")
        automaton.printStats()

        logger.debug("Cleaning outDir = '$outDir'...")
        outDir.walkBottomUp().forEach { file ->
            if (file != outDir) {
                file.delete()
            }
        }

        automaton.dump(outDir, name = "ControllerX${controllerIndex}")

        val hppFile = outDir.resolve("generated_Controller${controllerIndex}.hpp")
        hppFile.ensureParentExists().sink().buffer().useWith {
            writeln(automaton.toMutexControllerCppString(
                controllerName = "ControllerX${controllerIndex}",
                outputNamesBoolean = outputNamesBoolean,
                outputNamesIntWithDomain = outputNamesIntWithDomain
            ))
        }

        if (automaton.verify(scenarioTree))
            logger.info("Verify: OK")
        else {
            logger.error("Verify: FAILED")
        }
    } else {
        logger.error("Inference failed")
    }

    logger.info("End time: ${DateTime.nowLocal().format(dateTimeFormat)}")
    logger.info("All done in %.3f seconds".format(timeSince(timeStart).seconds))
}
