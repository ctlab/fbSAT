package ru.ifmo.fbsat.cli

import okio.buffer
import okio.sink
import ru.ifmo.fbsat.core.scenario.NuSmvTrace
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.write
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

fun main() {
    // val dir = File("data/sim-lily19")
    val dir = File("data/sim-lily21")
    val fileInput = dir.resolve("traces")
    val fileOutput = dir.resolve("scenarios")
    val fileInputNames = dir.resolve("input-names")
    val fileOutputNames = dir.resolve("output-names")
    val inputNames = fileInputNames.readLines()
    val outputNames = fileOutputNames.readLines()

    val traces = NuSmvTrace.fromFile(fileInput)
    log.info("Total traces: ${traces.size}")
    log.info("Traces lengths: ${traces.map { it.states.size }}")

    val scenarios = traces.map { trace ->
        PositiveScenario.fromTrace(trace, inputNames, outputNames)
    }
    log.info("Total scenario elements: ${scenarios.sumBy { it.elements.size }}")

    log.info("Dumping scenarios to '$fileOutput'...")
    fileOutput.sink().buffer().useWith {
        writeln("${scenarios.size}")
        for (scenario in scenarios) {
            for (element in scenario.elements) {
                write("in=${element.inputEvent!!.name}[${element.inputValues.values.toBinaryString()}]; ")
                write("out=${element.outputEvent!!.name}[${element.outputValues.values.toBinaryString()}]; ")
            }
            writeln("")
        }
    }

    val tree = ScenarioTree.fromFile(fileOutput, inputNames, outputNames)
    log.info("Tree size: ${tree.size}")

    log.success("All done!")
}
