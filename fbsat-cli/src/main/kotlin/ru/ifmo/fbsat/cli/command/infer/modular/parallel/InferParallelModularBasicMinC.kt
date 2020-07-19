@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.modular.parallel

import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.required
import ru.ifmo.fbsat.cli.command.infer.options.AUTOMATON_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.ExtraOptions
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.cli.command.infer.options.endNumberOfStatesOption
import ru.ifmo.fbsat.cli.command.infer.options.inputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfModulesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.outputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.scenariosFileOption
import ru.ifmo.fbsat.cli.command.infer.options.startNumberOfStatesOption
import ru.ifmo.fbsat.core.automaton.ParallelModularAutomaton
import ru.ifmo.fbsat.core.task.modular.basic.parallel.parallelModularBasicMinC
import java.io.File

private class ParallelModularBasicMinCInputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val outDir: File by outDirOption()
    val inputNames: List<String> by inputNamesOption()
    val outputNames: List<String> by outputNamesOption()
}

private class ParallelModularBasicMinCAutomatonOptions : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfModules: Int by numberOfModulesOption().required()
    val startNumberOfStates: Int by startNumberOfStatesOption().default(1)
    val endNumberOfStates: Int by endNumberOfStatesOption().default(20)
}

class InferParallelModularBasicMinCCommand :
    AbstractInferParallelModularCommand("modular-parallel-basic-minC") {
    private val io by ParallelModularBasicMinCInputOutputOptions()
    private val params by ParallelModularBasicMinCAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val outDir: File get() = io.outDir

    override fun infer(): ParallelModularAutomaton? =
        inferrer.parallelModularBasicMinC(
            scenarioTree = oldTree,
            numberOfModules = params.numberOfModules,
            start = params.startNumberOfStates,
            end = params.endNumberOfStates
        )
}
