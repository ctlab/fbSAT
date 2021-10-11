package ru.ifmo.fbsat.cli.command.infer.distributed

import com.github.ajalt.clikt.core.ParameterHolder
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.convert
import com.github.ajalt.clikt.parameters.options.multiple
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.file
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.cli.command.infer.options.AUTOMATON_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.ExtraOptions
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.cli.command.infer.options.maxGuardSizeOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfModulesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.smvDirOption
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.task.distributed.complete.distributedCegis2
import ru.ifmo.fbsat.core.utils.toMultiArray
import java.io.File

fun ParameterHolder.multiScenariosFileOption() =
    option(
        "-i", "--scenarios",
        help = "File with scenarios",
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
        help = "File with input variables names",
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
        help = "File with output variables names",
        metavar = "<path>"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    ).convert {
        it.readLines()
    }.multiple(required = true)

private class DistributedCegis2InputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val multiScenariosFile: List<File> by multiScenariosFileOption()
    val moduleNames: List<String> by moduleNamesOption()
    val multiInputNames: List<List<String>> by multiInputNamesOption()
    val multiOutputNames: List<List<String>> by multiOutputNamesOption()
    val outDir: File by outDirOption()
    val smvDir: File by smvDirOption()
}

private class DistributedCegis2AutomatonOptions : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfModules: Int by numberOfModulesOption().required()
    val multiMaxGuardSize: List<Int> by maxGuardSizeOption().multiple(required = true)
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
        io.multiInputNames.toMultiArray()
    }
    override val modularOutputNames: MultiArray<List<String>> by lazy {
        io.multiOutputNames.toMultiArray()
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
            smvDir = io.smvDir
        )
    }
}
