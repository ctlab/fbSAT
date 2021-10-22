package ru.ifmo.fbsat.cli.entry.braccio.mutex

import com.github.lipen.satlib.solver.GlucoseSolver
import com.github.lipen.satlib.solver.Solver
import com.soywiz.klock.DateTime
import com.soywiz.klock.PerformanceCounter
import okio.buffer
import okio.source
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
import ru.ifmo.fbsat.core.task.single.basic.basicMin
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.lineSequence
import ru.ifmo.fbsat.core.utils.timeSince
import java.io.File

private val logger = MyLogger {}

private fun String.toBool(): Boolean = when (lowercase()) {
    "0", "false" -> false
    "1", "true" -> true
    else -> error("Bad non-boolean string '$this'")
}

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
                ?: error("Bad string 's' doesn't match a regex")
            val (iter, index, rest) = m.destructured
            val data = rest.split(" ").associate {
                val (name, value) = it.split("=", limit = 2).map(String::trim)
                name to value
            }
            return LogLine(iter.toInt(), index.toInt(), data)
        }
    }
}

private fun readScenarioFromLog(
    file: File,
    inputNames: List<String>,
    outputNames: List<String>,
    outputNamesBoolean: List<String>,
    outputNamesIntWithDomain: List<Pair<String, List<Int>>>,
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
        .filter { it.index == 1 }
        .zipWithNext { a, b ->
            val outputValuesBoolean = outputNamesBoolean.associateWith { name ->
                b.data.getValue(name).toBool()
            }
            val outputValuesInt = outputNamesIntWithDomain.flatMap { (name, domain) ->
                domain.map { v ->
                    Pair("${name}_$v", (b.data.getValue(name).toInt() == v))
                }
            }.toMap()
            val map = outputValuesBoolean + outputValuesInt
            val outputValues = outputNames.map { map.getValue(it) }
            ScenarioElement(
                InputAction(
                    event = InputEvent("REQ"),
                    values = InputValues(inputNames.map { a.data.getValue(it).toBool() })
                ),
                OutputAction(
                    event = OutputEvent("CNF"),
                    values = OutputValues(outputValues)
                )
            )
        }
    return PositiveScenario(elements.preprocessed)
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
    val inputNames = listOf(
        "want_cargo_on_out",
        "is_acquired",
        "is_done_m1",
        "is_done_m2",
        "is_done_m3",
        "is_done_m4",
        "is_done_m5",
        "is_done_m6",
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
    val outputNamesInt =
        anglesBase.map { "go_base_$it" } +
            anglesShoulder.map { "go_shoulder_$it" } +
            anglesElbow.map { "go_elbow_$it" } +
            anglesWristVer.map { "go_wrist_ver_$it" } +
            anglesWristRot.map { "go_wrist_rot_$it" } +
            anglesGripper.map { "go_gripper_$it" }
    val outputNames = outputNamesBoolean + outputNamesInt

    val fileLog = File("data/braccio/mutex/serial_mutex_i2c_button1.log")
    val scenario = readScenarioFromLog(
        file = fileLog,
        inputNames = inputNames,
        outputNames = outputNames,
        outputNamesBoolean = outputNamesBoolean,
        outputNamesIntWithDomain = listOf(
            "go_base" to anglesBase,
            "go_shoulder" to anglesShoulder,
            "go_elbow" to anglesElbow,
            "go_wrist_ver" to anglesWristVer,
            "go_wrist_rot" to anglesWristRot,
            "go_gripper" to anglesGripper,
        )
    )
    val scenarios = listOf(scenario)
    val scenarioTree = PositiveScenarioTree.fromScenarios(
        scenarios = scenarios,
        inputNames = inputNames,
        outputNames = outputNamesBoolean,
        inputEvents = listOf(InputEvent("REQ")),
        outputEvents = listOf(OutputEvent("CNF"))
    )
    scenarioTree.printStats()

    // infer
    val automaton = inferrer.basicMin(scenarioTree)

    if (automaton != null) {
        logger.info("Inferred automaton:")
        automaton.pprint()
        logger.info("Inferred automaton has:")
        automaton.printStats()
    } else {
        logger.error("Inference failed")
    }

    logger.info("End time: ${DateTime.nowLocal().format(dateTimeFormat)}")
    logger.info("All done in %.3f seconds".format(timeSince(timeStart).seconds))
}
