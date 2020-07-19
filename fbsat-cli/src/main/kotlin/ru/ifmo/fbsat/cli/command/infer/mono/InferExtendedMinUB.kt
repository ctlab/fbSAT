@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.mono

import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.default
import ru.ifmo.fbsat.cli.command.infer.options.AUTOMATON_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.ExtraOptions
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.cli.command.infer.options.endMaxGuardSizeOption
import ru.ifmo.fbsat.cli.command.infer.options.inputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.maxPlateauWidthOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfStatesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.outputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.scenariosFileOption
import ru.ifmo.fbsat.cli.command.infer.options.startMaxGuardSizeOption
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.task.single.extended.extendedMinUB
import java.io.File

private class ExtendedMinUBInputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val outDir: File by outDirOption()
    val inputNames: List<String> by inputNamesOption()
    val outputNames: List<String> by outputNamesOption()
}

private class ExtendedMinUBAutomatonOptions : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfStates: Int? by numberOfStatesOption()
    val startMaxGuardSize: Int by startMaxGuardSizeOption().default(1)
    val endMaxGuardSize: Int by endMaxGuardSizeOption().default(20)
    val maxPlateauWidth: Int? by maxPlateauWidthOption()
}

class InferExtendedMinUBCommand : AbstractInferMonoCommand("extended-min-ub") {
    private val io by ExtendedMinUBInputOutputOptions()
    private val params by ExtendedMinUBAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val outDir: File get() = io.outDir

    override fun infer(): Automaton? =
        inferrer.extendedMinUB(
            scenarioTree = oldTree,
            numberOfStates = params.numberOfStates,
            start = params.startMaxGuardSize,
            end = params.endMaxGuardSize,
            maxPlateauWidth = params.maxPlateauWidth
        )
}
