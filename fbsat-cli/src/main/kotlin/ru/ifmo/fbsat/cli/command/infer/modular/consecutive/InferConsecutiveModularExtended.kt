@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.modular.consecutive

import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.required
import ru.ifmo.fbsat.cli.command.infer.options.AUTOMATON_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.ExtraOptions
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.cli.command.infer.options.getInitialOutputValuesOption
import ru.ifmo.fbsat.cli.command.infer.options.inputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.maxGuardSizeOption
import ru.ifmo.fbsat.cli.command.infer.options.maxOutgoingTransitionsOption
import ru.ifmo.fbsat.cli.command.infer.options.maxTotalGuardsSizeOption
import ru.ifmo.fbsat.cli.command.infer.options.maxTransitionsOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfModulesOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfStatesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.outputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.scenariosFileOption
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.consecutiveModularExtended
import java.io.File

private class ConsecutiveModularExtendedInputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val outDir: File by outDirOption()
    val inputNames: List<String> by inputNamesOption()
    val outputNames: List<String> by outputNamesOption()
    val initialOutputValues: OutputValues? by getInitialOutputValuesOption()
}

private class ConsecutiveModularExtendedAutomatonOptions : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfModules: Int by numberOfModulesOption().required()
    val numberOfStates: Int by numberOfStatesOption().required()
    val maxOutgoingTransitions: Int? by maxOutgoingTransitionsOption()
    val maxGuardSize: Int by maxGuardSizeOption().required()
    val maxTransitions: Int? by maxTransitionsOption()
    val maxTotalGuardsSize: Int? by maxTotalGuardsSizeOption()
}

class InferConsecutiveModularExtendedCommand :
    AbstractInferConsecutiveModularCommand("modular-consecutive-extended") {
    private val io by ConsecutiveModularExtendedInputOutputOptions()
    private val params by ConsecutiveModularExtendedAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val initialOutputValues: OutputValues? get() = io.initialOutputValues
    override val outDir: File get() = io.outDir

    override fun infer(): ConsecutiveModularAutomaton? =
        inferrer.consecutiveModularExtended(
            scenarioTree = scenarioTree,
            numberOfModules = params.numberOfModules,
            numberOfStates = params.numberOfStates,
            maxOutgoingTransitions = params.maxOutgoingTransitions,
            maxGuardSize = params.maxGuardSize,
            maxTransitions = params.maxTransitions,
            maxTotalGuardsSize = params.maxTotalGuardsSize,
            isEncodeReverseImplication = extraOptions.isEncodeReverseImplication
        )
}
