@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.distributed

import com.github.ajalt.clikt.core.ParameterHolder
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.convert
import com.github.ajalt.clikt.parameters.options.multiple
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.cli.command.infer.options.AUTOMATON_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.ExtraOptions
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.cli.command.infer.options.getInitialOutputValuesOption
import ru.ifmo.fbsat.cli.command.infer.options.maxGuardSizeOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfModulesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.smvDirOption
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.task.distributed.complete.distributedCegis2
import ru.ifmo.fbsat.core.utils.multiArrayOfNulls
import ru.ifmo.fbsat.core.utils.toMultiArray
import java.io.File

fun ParameterHolder.multiScenariosFileOption() =
    option(
        "-i", "--scenarios",
        help = "File with scenarios (can be passed multiple times)",
        metavar = "<path>"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    ).multiple(required = true)

fun ParameterHolder.moduleNamesOption() =
    option(
        "--module-names",
        help = "File with module names",
        metavar = "<path>"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    ).convert {
        it.readLines()
    }.required()

fun ParameterHolder.multiInputNamesOption() =
    option(
        "--input-names",
        help = "File with input variables names (can be passed multiple times)",
        metavar = "<path>"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    ).convert {
        it.readLines()
    }.multiple(required = true)

fun ParameterHolder.multiOutputNamesOption() =
    option(
        "--output-names",
        help = "File with output variables names (can be passed multiple times)",
        metavar = "<path>"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    ).convert {
        it.readLines()
    }.multiple(required = true)

fun ParameterHolder.startDOption() =
    option(
        "-Dstart",
        help = "Start D value",
        metavar = "<int>"
    ).int()

private class DistributedCegis2InputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val multiScenariosFile: List<File> by multiScenariosFileOption()
    val moduleNames: List<String> by moduleNamesOption()
    val multiInputNames: List<List<String>> by multiInputNamesOption()
    val multiOutputNames: List<List<String>> by multiOutputNamesOption()
    val multiInitialOutputValues: List<OutputValues> by getInitialOutputValuesOption().multiple()
    val outDir: File by outDirOption()
    val smvDir: File by smvDirOption()
}

private class DistributedCegis2AutomatonOptions : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfModules: Int by numberOfModulesOption().required()
    val multiMaxGuardSize: List<Int> by maxGuardSizeOption().multiple(required = true)
    val startD: Int? by startDOption()
}

class InferDistributedCegis2Command : AbstractInferDistributedModularCommand("distributed-cegis2") {
    private val io by DistributedCegis2InputOutputOptions()
    private val params by DistributedCegis2AutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val numberOfModules: Int get() = params.numberOfModules
    override val modularScenariosFile: MultiArray<File> by lazy {
        io.multiScenariosFile.toMultiArray()
    }
    override val modularInputNames: MultiArray<List<String>> by lazy {
        when (io.multiInputNames.size) {
            0 -> error("--input-names is a required option")
            1 -> MultiArray.new(numberOfModules) { io.multiInputNames[0] }
            else -> io.multiInputNames.toMultiArray()
        }
    }
    override val modularOutputNames: MultiArray<List<String>> by lazy {
        when (io.multiOutputNames.size) {
            0 -> error("--output-names is a required option")
            1 -> MultiArray.new(numberOfModules) { io.multiOutputNames[0] }
            else -> io.multiOutputNames.toMultiArray()
        }
    }
    override val modularInitialOutputValues: MultiArray<OutputValues?> by lazy {
        when (io.multiInitialOutputValues.size) {
            0 -> multiArrayOfNulls(numberOfModules)
            1 -> MultiArray.new(numberOfModules) { io.multiInitialOutputValues[0] }
            else -> io.multiInitialOutputValues.toMultiArray()
        }
    }
    override val outDir: File get() = io.outDir

    @Suppress("LocalVariableName")
    override fun infer(): DistributedAutomaton? {
        val M = numberOfModules
        return inferrer.distributedCegis2(
            numberOfModules = M,
            modularScenarioTree = modularScenarioTree,
            modularModuleName = io.moduleNames.toMultiArray(),
            modularInputEvents = MultiArray.new(M) {
                listOf("REQ").map { InputEvent(it) }
            },
            modularOutputEvents = MultiArray.new(M) {
                listOf("CNF").map { OutputEvent(it) }
            },
            modularInputNames = modularInputNames,
            modularOutputNames = modularOutputNames,
            modularMaxGuardSize = params.multiMaxGuardSize.toMultiArray(),
            smvDir = io.smvDir,
            startD = params.startD ?: 1,
        )
    }
}
