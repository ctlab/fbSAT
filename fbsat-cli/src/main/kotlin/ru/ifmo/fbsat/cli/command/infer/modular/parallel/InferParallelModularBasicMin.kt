@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.modular.parallel

import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.required
import ru.ifmo.fbsat.cli.command.infer.options.AUTOMATON_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.ExtraOptions
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.cli.command.infer.options.getInitialOutputValuesOption
import ru.ifmo.fbsat.cli.command.infer.options.inputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.maxOutgoingTransitionsOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfModulesOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfStatesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.outputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.scenariosFileOption
import ru.ifmo.fbsat.core.automaton.ParallelModularAutomaton
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.task.modular.basic.parallel.parallelModularBasicMin
import java.io.File

private class ParallelModularBasicMinInputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val outDir: File by outDirOption()
    val inputNames: List<String> by inputNamesOption()
    val outputNames: List<String> by outputNamesOption()
    val initialOutputValues: OutputValues? by getInitialOutputValuesOption()
}

private class ParallelModularBasicMinAutomatonOptions : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfModules: Int by numberOfModulesOption().required()
    val numberOfStates: Int? by numberOfStatesOption()
    val maxOutgoingTransitions: Int? by maxOutgoingTransitionsOption()
}

class InferParallelModularBasicMinCommand :
    AbstractInferParallelModularCommand("modular-parallel-basic-min") {
    private val io by ParallelModularBasicMinInputOutputOptions()
    private val params by ParallelModularBasicMinAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val initialOutputValues: OutputValues? get() = io.initialOutputValues
    override val outDir: File get() = io.outDir

    override fun infer(): ParallelModularAutomaton? =
        inferrer.parallelModularBasicMin(
            scenarioTree = scenarioTree,
            numberOfModules = params.numberOfModules,
            numberOfStates = params.numberOfStates,
            maxOutgoingTransitionsForC = { params.maxOutgoingTransitions ?: it }
        )
}
