package ru.ifmo.fbsat.core.task.extra.schema.stateful

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.negative.Counterexample
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.timeSince
import java.io.File

private val logger = MyLogger {}

private fun readTree(
    fileName: String,
    inputNames: List<String>,
    outputNames: List<String>,
): PositiveScenarioTree {
    val tree = PositiveScenarioTree(
        inputEvents = listOf(InputEvent("REQ")),
        outputEvents = listOf(OutputEvent("CNF")),
        inputNames = inputNames,
        outputNames = outputNames,
        isTrie = true // false
    )

    val traces = Counterexample.fromXml(File(fileName))
    logger.info("Traces: ${traces.size}")
    val scenarios = traces.map { trace ->
        PositiveScenario.fromAutoReqCnf(trace, inputNames, outputNames)
    }
    for (scenario in scenarios) {
        logger.info("Adding scenario of length ${scenario.elements.size}")
        tree.addScenario(scenario)
        if (tree.scenarios.size == 1) {
            for (element in scenario.elements) {
                println("[fold] $element")
            }
        }
    }
    logger.info("Tree size: ${tree.size}")
    logger.info("Tree input names: ${tree.inputNames}")
    logger.info("Tree output names: ${tree.outputNames}")

    return tree
}

private fun runAdding() {
    Globals.IS_DEBUG = true

    val inputNames = listOf("in_x", "in_y")
    val outputNames = listOf("out0", "out1")
    // // val outputNames = listOf("out0", "out1", "out2")
    // // val outputNames = listOf("out0", "out1", "out2", "out3")

    val X = inputNames.size
    val Z = outputNames.size

    val traceFilename = "data/counter/trace_adding2_10x30.xml"
    val tree = readTree(traceFilename, inputNames, outputNames)

    val blockTypes = listOf(
        GateBlock.AND,
        GateBlock.OR,
        GateBlock.NOT,
        GateBlock.HA,
        AutomatonBlock.CNT,
    )

    // val M = 10
    // if (synthesizeStatefulSystem(tree, M = M, X = X, Z = Z)) {
    //     logger.success("M = $M: SAT")
    // } else {
    //     logger.failure("M = $M: UNSAT")
    // }

    synthesizeStatefulSystemIterativelyBottomUp(tree, X = X, Z = Z, minM = 1, maxM = 10, blockTypes)
}

private fun main() {
    val timeStart = PerformanceCounter.reference

    runAdding()

    logger.info("All done in %.3f s.".format(timeSince(timeStart).seconds))
}
