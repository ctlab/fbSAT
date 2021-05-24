@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.cli.entry

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.solver.MiniSatSolver
import com.github.lipen.satlib.solver.Solver
import com.soywiz.klock.DateTime
import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.negative.readCounterexamplesFromFile
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.distributed.complete.distributedCegis2
import ru.ifmo.fbsat.core.utils.*
import java.io.File
import java.nio.file.Paths

private val logger = MyLogger {}

fun main() {
    val path = Paths.get("").toAbsolutePath().toFile()
    Globals.INITIAL_OUTPUT_VALUES = OutputValues.zeros(7)
    val positiveScenarioTree = PositiveScenarioTree.fromFile(path.resolve("data/gen/traces_10_20_01.gz"), inputNamesPnP, outputNamesPnP)
    val negativeScenarioTree = PositiveScenarioTree.fromFile(path.resolve("data/gen/traces_10_20_01.gz"), inputNamesPnP, outputNamesPnP)
    positiveScenarioTree.dump(path)
    negativeScenarioTree.dump(path)

}
