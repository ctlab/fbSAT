package ru.ifmo.fbsat.cli.entry.braccio.cargo

import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import okio.buffer
import okio.sink
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.ensureParentExists
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

private val logger = MyLogger {}

fun main() {
    val controllerIndex = 1
    val inputNames: List<String> = if (controllerIndex == 1) listOf(
        "want_cargo_on_Z1",
        "want_cargo_on_Z2",
        "cargo_on_Z1",
        "cargo_on_Z2",
        // "is_done_m1",
        // "is_done_m2",
        // "is_done_m3",
        // "is_done_m4",
        // "is_done_m5",
        // "is_done_m6",
        "is_done_all",
    ) else if (controllerIndex == 2) listOf(
        "cargo_is_processed_Z1",
        "cargo_is_processed_Z2",
        "is_done_all",
    ) else error("!")
    val outputNamesBoolean: List<String> = listOf()
    val anglesBase = listOf(0, 60, 90, 120, 180)
    val anglesShoulder = if (controllerIndex == 1)
        listOf(65, 80, 90, 115, 140)
    else if (controllerIndex == 2)
        listOf(65, 90, 100, 115, 140)
    else error("!")
    val anglesElbow = listOf(0, 180)
    val anglesWristVer = if (controllerIndex == 1)
        listOf(10, 15, 160, 165, 170)
    else if (controllerIndex == 2)
        listOf(10, 15, 165)
    else error("!")
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

    val logDir = "data/braccio/cargo/logs"
    val logFiles = listOf(
        "$logDir/serial_proc_i2c_but1_proc1.log",
        "$logDir/serial_proc_i2c_but2_proc2.log",
        "$logDir/serial_proc_i2c_but12_proc12.log",
        "$logDir/serial_proc_i2c_but12_proc12_loop1.log",
        "$logDir/serial_proc_i2c_but1_but1_but2_but2.log",
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
    // for ((i, scenario) in scenarios.withIndex()) {
    //     println("Scenario #${i+1}:")
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

    val outDir = File("out/braccio/cargo-scenarios")
    val json = Json {
        prettyPrint = true
    }

    for (i in logFiles.indices) {
        val scenario = scenarios[i]
        val fileScenario = outDir.resolve("scenario-X$controllerIndex-${i + 1}.json")
        logger.info("Serializing scenario #${i + 1} for X$controllerIndex to '$fileScenario'")
        fileScenario.ensureParentExists().writeText(json.encodeToString(scenario))
    }

    val fileAllScenarios = outDir.resolve("all-scenarios-X$controllerIndex.json")
    logger.info("Serializing all scenario for X$controllerIndex to '$fileAllScenarios'")
    fileAllScenarios.ensureParentExists().writeText(json.encodeToString(scenarios))

    outDir.resolve("input-names-X$controllerIndex").ensureParentExists().sink().buffer().useWith {
        for (name in inputNames) {
            writeln(name)
        }
    }
    outDir.resolve("output-names-X$controllerIndex").ensureParentExists().sink().buffer().useWith {
        for (name in outputNames) {
            writeln(name)
        }
    }

    logger.info("All done!")
}
