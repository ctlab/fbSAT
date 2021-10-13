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
import ru.ifmo.fbsat.cli.command.infer.options.getInitialOutputValuesOption
import ru.ifmo.fbsat.cli.command.infer.options.inputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.maxOutgoingTransitionsOption
import ru.ifmo.fbsat.cli.command.infer.options.maxTransitionsOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfModulesOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfStatesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.outputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.scenariosFileOption
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.task.distributed.basic.distributedBasic
import ru.ifmo.fbsat.core.utils.Globals
import java.io.File

private class DistributedBasicInputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val outDir: File by outDirOption()
    val inputNames: List<String> by inputNamesOption()
    val outputNames: List<String> by outputNamesOption()
    val initialOutputValues: OutputValues? by getInitialOutputValuesOption()
}

private class DistributedBasicAutomatonOptions : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfModules: Int by numberOfModulesOption().required()
    val numberOfStates: Int by numberOfStatesOption().required()
    val maxOutgoingTransitions: Int? by maxOutgoingTransitionsOption()
    val maxTransitions: Int? by maxTransitionsOption()
}

class InferDistributedBasicCommand : AbstractInferDistributedCommand("distributed-basic") {
    private val io by DistributedBasicInputOutputOptions()
    private val params by DistributedBasicAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val numberOfModules: Int get() = params.numberOfModules
    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val initialOutputValues: OutputValues? get() = io.initialOutputValues
    override val outDir: File get() = io.outDir

    @Suppress("LocalVariableName")
    override fun infer(): DistributedAutomaton? {
        val M = params.numberOfModules
        val C = params.numberOfStates
        val K = params.maxOutgoingTransitions
        val T = params.maxTransitions
        return inferrer.distributedBasic(
            numberOfModules = M,
            // compoundScenarioTree = compoundScenarioTree,
            modularScenarioTree = compoundScenarioTree.modular,
            modularNumberOfStates = MultiArray.new(M) { C },
            modularMaxOutgoingTransitions = MultiArray.new(M) { K },
            modularMaxTransitions = MultiArray.new(M) { T }, // for now
            modularIsEncodeReverseImplication = MultiArray.new(M) { extraOptions.isEncodeReverseImplication },
            // maxTransitions = T
        )
    }
}
