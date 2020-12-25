@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.modular.arbitrary

import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.required
import ru.ifmo.fbsat.cli.command.infer.options.AUTOMATON_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.ExtraOptions
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.cli.command.infer.options.inputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.maxGuardSizeOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfModulesOption
import ru.ifmo.fbsat.cli.command.infer.options.numberOfStatesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.outputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.scenariosFileOption
import ru.ifmo.fbsat.core.automaton.ArbitraryModularAutomaton
import ru.ifmo.fbsat.core.task.modular.extended.arbitrary.arbitraryModularExtendedMin
import java.io.File

private class ArbitraryModularExtendedMinInputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val outDir: File by outDirOption()
    val inputNames: List<String> by inputNamesOption()
    val outputNames: List<String> by outputNamesOption()
}

private class ArbitraryModularExtendedMinAutomatonOptions : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfModules: Int by numberOfModulesOption().required()
    val numberOfStates: Int? by numberOfStatesOption()
    val maxGuardSize: Int by maxGuardSizeOption().required()
}

class InferArbitraryModularExtendedMinCommand :
    AbstractInferArbitraryModularCommand("modular-arbitrary-extended-min") {

    private val io by ArbitraryModularExtendedMinInputOutputOptions()
    private val params by ArbitraryModularExtendedMinAutomatonOptions()
    override val solverOptions by SolverOptions()
    override val extraOptions by ExtraOptions()

    override val scenariosFile: File get() = io.scenariosFile
    override val inputNames: List<String> get() = io.inputNames
    override val outputNames: List<String> get() = io.outputNames
    override val outDir: File get() = io.outDir

    override fun infer(): ArbitraryModularAutomaton? =
        inferrer.arbitraryModularExtendedMin(
            scenarioTree = scenarioTree,
            numberOfModules = params.numberOfModules,
            numberOfStates = params.numberOfStates,
            maxGuardSize = params.maxGuardSize
        )
}

@Suppress("ClassName")
private object `infer modular-arbitrary-extended-min` {
    @JvmStatic
    fun main(args: Array<String>) {
        InferArbitraryModularExtendedMinCommand().main(listOf(
            "-i", "data/tests-1.gz",
            "-M", "2",
            "-P", "5",
            "--epsilon-output-events", "none",
            "--debug",
            "--minisat",
        ))
    }
}
