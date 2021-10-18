package ru.ifmo.fbsat.cli.command.slice

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.convert
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import okio.buffer
import okio.sink
import okio.source
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.getInitialOutputValuesOption
import ru.ifmo.fbsat.cli.command.infer.options.inputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.outputNamesOption
import ru.ifmo.fbsat.cli.command.infer.options.scenariosFileOption
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.ensureParentExists
import ru.ifmo.fbsat.core.utils.lineSequence
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.write
import ru.ifmo.fbsat.core.utils.writeln
import java.io.File

private val logger = MyLogger {}

private class SliceInputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val outDir: File by outDirOption()
    val inputNames: List<String> by inputNamesOption()
    val outputNames: List<String> by outputNamesOption()
    val initialOutputValues: OutputValues? by getInitialOutputValuesOption()
    val isStdout : Boolean by option(
        "--stdout",
        help = "Additionally print sliced scenarios to stdout"
    ).flag(
        "--no-stdout",
        default = false
    )
}

private sealed interface Vars {
    object All : Vars {
        override fun toString(): String = "All"
    }

    data class Some(val indices: List<Int>) : Vars
}

private class SliceParamsOptions : OptionGroup("Params Options") {
    val sliceInput: Vars by option(
        "-si",
        "--slice-input",
        help = "Comma-separated list of input variables (1-based) or 'all'"
    ).convert {
        if (it.lowercase() == "all") {
            Vars.All
        } else {
            Vars.Some(it.split(",").map { it.toInt() - 1 })
        }
    }.default(Vars.All)

    val sliceOutput: Vars by option(
        "-so",
        "--slice-output",
        help = "Comma-separated list of output variables (1-based) or 'all'"
    ).convert {
        if (it.lowercase() == "all") {
            Vars.All
        } else {
            Vars.Some(it.split(",").map { it.toInt() - 1 })
        }
    }.default(Vars.All)
}

private fun PositiveScenario.slice(sliceInput: Vars, sliceOutput: Vars): PositiveScenario {
    return PositiveScenario(elements = elements.map { it.slice(sliceInput, sliceOutput) })
}

private fun ScenarioElement.slice(sliceInput: Vars, sliceOutput: Vars): ScenarioElement {
    return ScenarioElement(
        inputAction = InputAction(
            event = inputEvent,
            values = inputValues.slice(sliceInput)
        ),
        outputAction = OutputAction(
            event = outputEvent,
            values = outputValues.slice(sliceOutput)
        )
    )
}

private fun InputValues.slice(sliceInput: Vars): InputValues {
    return when (sliceInput) {
        is Vars.All -> this
        is Vars.Some -> InputValues(values.slice(sliceInput.indices))
    }
}

private fun OutputValues.slice(sliceInput: Vars): OutputValues {
    return when (sliceInput) {
        is Vars.All -> this
        is Vars.Some -> OutputValues(values.slice(sliceInput.indices))
    }
}

class SliceCommand : CliktCommand("slice") {
    private val io by SliceInputOutputOptions()
    private val params by SliceParamsOptions()

    override fun run() {
        val scenarios = PositiveScenario.fromFile(
            file = io.scenariosFile,
            initialOutputValues = io.initialOutputValues ?: OutputValues.zeros(io.inputNames.size)
        )
        logger.info("Slicing scenarios: input = ${params.sliceInput}, output = ${params.sliceOutput}")
        val slicedScenarios = scenarios.map { it.slice(params.sliceInput, params.sliceOutput) }
        val outFile = io.outDir.resolve("sliced-scenarios")
        logger.info("Writing ${slicedScenarios.size} sliced scenario(s) to '$outFile'")
        outFile.ensureParentExists().sink().buffer().useWith {
            writeln("${slicedScenarios.size}")
            for (scenario in slicedScenarios) {
                for (element in scenario.elements) {
                    if (element.inputEvent != null) {
                        write("in=${element.inputEvent}[${element.inputValues.values.toBinaryString()}]; ")
                    }
                    if (element.outputEvent != null) {
                        write("out=${element.outputEvent}[${element.outputValues.values.toBinaryString()}]; ")
                    }
                }
                writeln()
            }
        }
        if (io.isStdout) {
            for (line in outFile.source().buffer().lineSequence()) {
                println(line)
            }
        }
    }
}
