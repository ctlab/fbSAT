@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.mono

import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.required
import ru.ifmo.fbsat.cli.command.infer.options.AUTOMATON_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.ExtraOptions
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.cli.command.infer.options.inputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.maxGuardSizeOption
import ru.ifmo.fbsat.cli.command.infer.options.maxOutgoingTransitionsOption
import ru.ifmo.fbsat.cli.command.infer.options.maxTotalGuardsSizeOption
import ru.ifmo.fbsat.cli.command.infer.options.maxTransitionsOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfStatesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.outputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.scenariosFileOption
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.task.single.extended.extended
import java.io.File

private class ExtendedInputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val outDir: File by outDirOption()
    val inputNames: List<String> by inputNamesOption()
    val outputNames: List<String> by outputNamesOption()
}

private class ExtendedAutomatonOptions : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfStates: Int by numberOfStatesOption().required()
    val maxOutgoingTransitions: Int? by maxOutgoingTransitionsOption()
    val maxGuardSize: Int by maxGuardSizeOption().required()
    val maxTransitions: Int? by maxTransitionsOption()
    val maxTotalGuardsSize: Int? by maxTotalGuardsSizeOption()
}

class InferExtendedCommand : AbstractInferMonoCommand("extended") {
    private val io by ExtendedInputOutputOptions()
    private val params by ExtendedAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val outDir: File get() = io.outDir

    override fun infer(): Automaton? =
        inferrer.extended(
            scenarioTree = oldTree,
            numberOfStates = params.numberOfStates,
            maxOutgoingTransitions = params.maxOutgoingTransitions,
            maxGuardSize = params.maxGuardSize,
            maxTransitions = params.maxTransitions,
            maxTotalGuardsSize = params.maxTotalGuardsSize,
            isEncodeReverseImplication = extraOptions.isEncodeReverseImplication
        )
}
