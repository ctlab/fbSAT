@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.cli.entry.braccio.cargo

import com.github.lipen.satlib.solver.CadicalSolver
import com.github.lipen.satlib.solver.Solver
import com.soywiz.klock.DateTime
import com.soywiz.klock.PerformanceCounter
import okio.buffer
import okio.sink
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.single.extended.extendedMin
import ru.ifmo.fbsat.core.utils.EpsilonOutputEvents
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.StartStateAlgorithms
import ru.ifmo.fbsat.core.utils.ensureParentExists
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

private val logger = MyLogger {}

fun main() {
    Globals.IS_DEBUG = true

    val dateTimeFormat = "yyyy-MM-dd HH:mm:ss"
    logger.info("Start time: ${DateTime.nowLocal().format(dateTimeFormat)}")
    val timeStart = PerformanceCounter.reference

    // val solver: Solver = GlucoseSolver()
    val solver: Solver = CadicalSolver()
    val outDir = File("out/braccio/cargo")
    val inferrer = Inferrer(solver, outDir)

    // input
    val controllerIndex = 1
    val inputNames: List<String> = listOf(
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
    )
    val outputNamesBoolean: List<String> = listOf(
        // "want_to_release",
        // "want_to_acquire",
    )
    val anglesBase = listOf(0, 60, 90, 120, 180)
    val anglesShoulder = listOf(65, 80, 90, 115, 140)
    val anglesElbow = listOf(0, 180)
    val anglesWristVer = listOf(10, 15, 160, 165, 170)
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
        // "$logDir/serial_proc_i2c_but2_proc2.log",
        // "$logDir/serial_proc_i2c_but12_proc12.log",
        // "$logDir/serial_proc_i2c_but12_proc12_loop1.log",
        // "$logDir/serial_proc_i2c_but1_but1_but2_but2.log",
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
    // val scenarioTree = PositiveScenarioTree.fromFile(
    //     file = File("out/braccio/cargo-scenarios/all-scenarios-X$controllerIndex.json"),
    //     inputNames = inputNames,
    //     outputNames = outputNames,
    //     inputEvents = listOf(InputEvent("REQ")),
    //     outputEvents = listOf(OutputEvent("CNF")),
    // )
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
    // val automaton: Automaton? = null
    // val automaton: Automaton? = inferrer.basic(
    //     scenarioTree,
    //     numberOfStates = 13,
    //     maxOutgoingTransitions = 3,
    //     maxTransitions = null,
    // )
    // val automaton: Automaton? = inferrer.basicMin(
    //     scenarioTree,
    //     start = 2,
    // )
    // val automaton: Automaton? = inferrer.extended(
    //     scenarioTree,
    //     numberOfStates = 13, // 13 is SAT
    //     maxOutgoingTransitions = 3,
    //     maxGuardSize = 5
    // )
    val automaton: Automaton? = inferrer.extendedMin(
        scenarioTree,
        // numberOfStates = 25, // ? is SAT
        maxOutgoingTransitionsForC = { 3 },
        maxGuardSize = 5,
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
            writeln(
                automaton.toCargoControllerCppString(
                    controllerName = "ControllerX${controllerIndex}",
                    outputNamesBoolean = outputNamesBoolean,
                    outputNamesIntWithDomain = outputNamesIntWithDomain
                )
            )
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
