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
import ru.ifmo.fbsat.core.scenario.preprocessed
import ru.ifmo.fbsat.core.task.Inferrer
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
    val outputNames = listOf(
        "go_base",
        "go_shoulder",
        "go_elbow",
        "go_wrist_ver",
        "go_wrist_rot",
        "go_gripper",
        "active",
        "want_to_release",
        "want_to_acquire",
    )
    val fileLog = File("data/braccio/mutex/serial_mutex_i2c_button1.log")
    val interestingLines = fileLog.source().buffer().lineSequence().filter {
        it.startsWith("[iter #")
    }.toList().let {
        // Remove last line (because it is probably corrupted)
        it.subList(0, it.size - 1)
    }
    for (line in interestingLines) {
        val logLine = LogLine.from(line)
        println(logLine)
    }
    val logLines = interestingLines.map { LogLine.from(it) }
    val elements = logLines
        .filter { it.index == 1 }
        .zipWithNext { a, b ->
            ScenarioElement(
                InputAction(
                    event = InputEvent("REQ"),
                    values = InputValues(inputNames.map { a.data.getValue(it).toBool() })
                ),
                OutputAction(
                    event = OutputEvent("CNF"),
                    values = OutputValues(outputNames.map { b.data.getValue(it).toBool() })
                )
            )
        }
    val scenario = PositiveScenario(elements.preprocessed)
    println("scenario = $scenario")

    // infer
    // TODO

    logger.info("End time: ${DateTime.nowLocal().format(dateTimeFormat)}")
    logger.info("All done in %.3f seconds".format(timeSince(timeStart).seconds))
}
