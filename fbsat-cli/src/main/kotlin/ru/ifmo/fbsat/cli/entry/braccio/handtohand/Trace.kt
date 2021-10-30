package ru.ifmo.fbsat.cli.entry.braccio.handtohand

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
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.all
import ru.ifmo.fbsat.core.utils.lineSequence
import java.io.File

private val logger = MyLogger {}

fun String.toBool(): Boolean = when (lowercase()) {
    "0", "false" -> false
    "1", "true" -> true
    else -> error("Bad non-boolean string '$this'")
}

val elementData: MutableMap<ScenarioElement, Any> = mutableMapOf()

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

fun readScenarioFromLog(
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
                    if (!x.data.containsKey(name)) {
                        logger.error("x.data missing key '$name'")
                    }
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
