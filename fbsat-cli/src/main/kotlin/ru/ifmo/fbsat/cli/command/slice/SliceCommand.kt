package ru.ifmo.fbsat.cli.command.slice

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.groups.provideDelegate
import com.github.ajalt.clikt.parameters.options.convert
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.int
import okio.buffer
import okio.sink
import okio.source
import ru.ifmo.fbsat.cli.command.infer.options.INPUT_OUTPUT_OPTIONS
import ru.ifmo.fbsat.cli.command.infer.options.getInitialOutputValuesOption
import ru.ifmo.fbsat.cli.command.infer.options.outDirOption
import ru.ifmo.fbsat.cli.command.infer.options.scenariosFileOption
import ru.ifmo.fbsat.core.scenario.OutputValues
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

@Suppress("PropertyName")
private class SliceInputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val outDir: File by outDirOption()
    val numberOfInputVariables: Int by option(
        "-X",
        help = "Number of input variables"
    ).int().required()
    val numberOfOutputVariables: Int by option(
        "-Z",
        help = "Number of output variables"
    ).int().required()
    val initialOutputValues: OutputValues? by getInitialOutputValuesOption()
    val isStdout: Boolean by option(
        "--stdout",
        help = "Additionally print sliced scenarios to stdout"
    ).flag(
        "--no-stdout",
        default = false
    )
}

private class SliceParamsOptions : OptionGroup("Slice Options") {
    val sliceInput: Vars by option(
        "-si",
        "--slice-input",
        help = "Comma-separated list of input variables (1-based) or 'all'"
    ).convert {
        if (it.lowercase() == "all") {
            Vars.All
        } else {
            Vars.Some(it.split(",").map(String::toInt))
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
            Vars.Some(it.split(",").map(String::toInt))
        }
    }.default(Vars.All)
}

private sealed interface Vars {
    object All : Vars {
        override fun toString(): String = "All"
    }

    data class Some(val variables: List<Int>) : Vars
}

class SliceCommand : CliktCommand("slice") {
    private val io by SliceInputOutputOptions()
    private val params by SliceParamsOptions()

    override fun run() {
        val scenarios = PositiveScenario.fromFile(
            file = io.scenariosFile,
            initialOutputValues = io.initialOutputValues ?: OutputValues.zeros(io.numberOfOutputVariables)
        )
        logger.info("Slicing scenarios: input = ${params.sliceInput}, output = ${params.sliceOutput}")
        val indicesInput = when (val s = params.sliceInput) {
            is Vars.All -> 0 until io.numberOfInputVariables
            is Vars.Some -> s.variables.map { it - 1 }
        }
        val indicesOutput = when (val s = params.sliceOutput) {
            is Vars.All -> 0 until io.numberOfOutputVariables
            is Vars.Some -> s.variables.map { it - 1 }
        }
        val slicedScenarios = scenarios.map {
            it.slice(indicesInput, indicesOutput)
        }
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
