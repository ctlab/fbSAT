@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.modular.consecutive

import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.required
import ru.ifmo.fbsat.cli.command.infer.options.AUTOMATON_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.ExtraOptions
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.cli.command.infer.options.inputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.maxOutgoingTransitionsOption
import ru.ifmo.fbsat.cli.command.infer.options.maxTransitionsOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfModulesOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfStatesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.outputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.scenariosFileOption
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.consecutiveModularBasic
import java.io.File

private class ConsecutiveModularBasicInputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val outDir: File by outDirOption()
    val inputNames: List<String> by inputNamesOption()
    val outputNames: List<String> by outputNamesOption()
}

private class ConsecutiveModularBasicAutomatonOptions : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfModules: Int by numberOfModulesOption().required()
    val numberOfStates: Int by numberOfStatesOption().required()
    val maxOutgoingTransitions: Int? by maxOutgoingTransitionsOption()
    val maxTransitions: Int? by maxTransitionsOption()
}

class InferConsecutiveModularBasicCommand :
    AbstractInferConsecutiveModularCommand("modular-consecutive-basic") {
    private val io by ConsecutiveModularBasicInputOutputOptions()
    private val params by ConsecutiveModularBasicAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val outDir: File get() = io.outDir

    override fun infer(): ConsecutiveModularAutomaton? =
        inferrer.consecutiveModularBasic(
            scenarioTree = oldTree,
            numberOfModules = params.numberOfModules,
            numberOfStates = params.numberOfStates,
            maxOutgoingTransitions = params.maxOutgoingTransitions,
            maxTransitions = params.maxTransitions,
            isEncodeReverseImplication = extraOptions.isEncodeReverseImplication
        )
}
