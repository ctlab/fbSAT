@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.distributed

import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.required
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.cli.command.infer.options.AUTOMATON_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.ExtraOptions
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.cli.command.infer.options.inputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfModulesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.outputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.scenariosFileOption
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.task.distributed.basic.distributedBasicMinC
import java.io.File

private class DistributedBasicMinCInputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val outDir: File by outDirOption()
    val inputNames: List<String> by inputNamesOption()
    val outputNames: List<String> by outputNamesOption()
}

private class DistributedBasicMinCAutomatonOptions : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfModules: Int by numberOfModulesOption().required()
}

class InferDistributedBasicMinCCommand : AbstractInferDistributedCommand("distributed-basic-min") {
    private val io by DistributedBasicMinCInputOutputOptions()
    private val params by DistributedBasicMinCAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val numberOfModules: Int get() = params.numberOfModules
    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val outDir: File get() = io.outDir

    override fun infer(): DistributedAutomaton? {
        val M = params.numberOfModules
        return inferrer.distributedBasicMinC(
            numberOfModules = M,
            compoundScenarioTree = compoundScenarioTree,
            modularScenarioTree = compoundScenarioTree.modular,
            modularIsEncodeReverseImplication = MultiArray.new(M) { extraOptions.isEncodeReverseImplication }
        )
    }
}
