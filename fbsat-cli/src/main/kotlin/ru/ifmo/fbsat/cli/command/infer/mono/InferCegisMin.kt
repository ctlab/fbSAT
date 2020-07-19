@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.mono

import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import ru.ifmo.fbsat.cli.command.infer.options.AUTOMATON_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.ExtraOptions
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.cli.command.infer.options.inputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.maxGuardSizeOption
import ru.ifmo.fbsat.cli.command.infer.options.maxPlateauWidthOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfStatesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.outputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.scenariosFileOption
import ru.ifmo.fbsat.cli.command.infer.options.smvDirOption
import ru.ifmo.fbsat.cli.command.infer.options.startNumberOfStatesOption
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.task.single.complete.cegisMin
import java.io.File

private class CegisMinInputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val outDir: File by outDirOption()
    val inputNames: List<String> by inputNamesOption()
    val outputNames: List<String> by outputNamesOption()
    val smvDir: File by smvDirOption()
}

private class CegisMinAutomatonOptions : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfStates: Int? by numberOfStatesOption()
    val startNumberOfStates: Int? by startNumberOfStatesOption()
    val maxGuardSize: Int? by maxGuardSizeOption()
    val maxPlateauWidth: Int? by maxPlateauWidthOption()
}

class InferCegisMinCommand : AbstractInferMonoCommand("cegis-min") {
    private val io by CegisMinInputOutputOptions()
    private val params by CegisMinAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val outDir: File get() = io.outDir

    override fun infer(): Automaton? =
        inferrer.cegisMin(
            scenarioTree = oldTree,
            initialNegativeScenarioTree = null,
            numberOfStates = params.numberOfStates,
            startNumberOfStates = params.startNumberOfStates,
            maxGuardSize = params.maxGuardSize,
            maxPlateauWidth = params.maxPlateauWidth,
            smvDir = io.smvDir
        )
}
