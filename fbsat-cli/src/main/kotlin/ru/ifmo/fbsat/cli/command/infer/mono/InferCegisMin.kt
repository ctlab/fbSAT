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
import ru.ifmo.fbsat.core.task.single.complete.cegisMinAssumptions
import ru.ifmo.fbsat.core.task.single.complete.cegisMinNikita
import ru.ifmo.fbsat.core.task.single.complete.cegisMinCutTree
import ru.ifmo.fbsat.core.task.single.complete.cegisMinHeight
import ru.ifmo.fbsat.core.task.single.complete.cegisicMin
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
            scenarioTree = scenarioTree,
            initialNegativeScenarioTree = null,
            numberOfStates = params.numberOfStates,
            startNumberOfStates = params.startNumberOfStates,
            maxGuardSize = params.maxGuardSize,
            maxPlateauWidth = params.maxPlateauWidth,
            smvDir = io.smvDir
        )
}

class InferCegisMinAssumptionsCommand : AbstractInferMonoCommand("cegis-min-assumptions") {
    private val io by CegisMinInputOutputOptions()
    private val params by CegisMinAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val outDir: File get() = io.outDir

    override fun infer(): Automaton? =
        inferrer.cegisMinAssumptions(
            scenarioTree = scenarioTree,
            initialNegativeScenarioTree = null,
            numberOfStates = params.numberOfStates,
            startNumberOfStates = params.startNumberOfStates,
            maxGuardSize = params.maxGuardSize,
            maxPlateauWidth = params.maxPlateauWidth,
            smvDir = io.smvDir
        )
}

class InferCegisMinNikitaCommand : AbstractInferMonoCommand("cegis-min-nikita") {
    private val io by CegisMinInputOutputOptions()
    private val params by CegisMinAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val outDir: File get() = io.outDir

    override fun infer(): Automaton? =
        inferrer.cegisMinNikita(
            scenarioTree = scenarioTree,
            initialNegativeScenarioTree = null,
            numberOfStates = params.numberOfStates,
            startNumberOfStates = params.startNumberOfStates,
            maxGuardSize = params.maxGuardSize,
            maxPlateauWidth = params.maxPlateauWidth,
            smvDir = io.smvDir
        )
}

class InferCegisMinCutTreeCommand : AbstractInferMonoCommand("cegis-min-cut-tree") {
    private val io by CegisMinInputOutputOptions()
    private val params by CegisMinAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val outDir: File get() = io.outDir

    override fun infer(): Automaton? =
        inferrer.cegisMinCutTree(
            scenarioTree = scenarioTree,
            initialNegativeScenarioTree = null,
            numberOfStates = params.numberOfStates,
            startNumberOfStates = params.startNumberOfStates,
            maxGuardSize = params.maxGuardSize,
            maxPlateauWidth = params.maxPlateauWidth,
            smvDir = io.smvDir
        )
}

class InferCegisMinHeightCommand : AbstractInferMonoCommand("cegis-min-height") {
    private val io by CegisMinInputOutputOptions()
    private val params by CegisMinAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val outDir: File get() = io.outDir

    override fun infer(): Automaton? =
        inferrer.cegisMinHeight(
            scenarioTree = scenarioTree,
            initialNegativeScenarioTree = null,
            numberOfStates = params.numberOfStates,
            startNumberOfStates = params.startNumberOfStates,
            maxGuardSize = params.maxGuardSize,
            maxPlateauWidth = params.maxPlateauWidth,
            smvDir = io.smvDir
        )
}

class InferCegisicMinCommand : AbstractInferMonoCommand("cegisic-min") {
    private val io by CegisMinInputOutputOptions()
    private val params by CegisMinAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val outDir: File get() = io.outDir

    override fun infer(): Automaton? =
        inferrer.cegisicMin(
            scenarioTree = scenarioTree,
            initialNegativeScenarioTree = null,
            numberOfStates = params.numberOfStates,
            startNumberOfStates = params.startNumberOfStates,
            maxGuardSize = params.maxGuardSize,
            maxPlateauWidth = params.maxPlateauWidth,
            smvDir = io.smvDir
        )
}
