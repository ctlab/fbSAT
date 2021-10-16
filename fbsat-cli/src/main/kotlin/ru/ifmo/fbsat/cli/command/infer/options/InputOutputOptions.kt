@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.options

import com.github.ajalt.clikt.core.ParameterHolder
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.options.convert
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.defaultLazy
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.file
import com.soywiz.klock.DateTime
import ru.ifmo.fbsat.core.scenario.negative.Counterexample
import ru.ifmo.fbsat.core.utils.inputNamesPnP
import ru.ifmo.fbsat.core.utils.outputNamesPnP
import java.io.File

internal const val INPUT_OUTPUT_OPTIONS = "Input/Output Options"

@Deprecated("This is just a template with ALL options. Create your own class.")
@Suppress("MemberVisibilityCanBePrivate", "unused")
class InputOutputOptions : OptionGroup(INPUT_OUTPUT_OPTIONS) {
    val scenariosFile: File by scenariosFileOption()
    val initialCounterexamples: List<Counterexample>? by initialCounterexamplesOption()
    val outDir: File by outDirOption()
    val smvDir: File by smvDirOption()
    val inputNames: List<String> by inputNamesOption()
    val outputNames: List<String> by outputNamesOption()
}

fun ParameterHolder.scenariosFileOption() =
    option(
        "-i", "--scenarios",
        help = "File with scenarios",
        metavar = "<path>"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    ).required()

fun ParameterHolder.initialCounterexamplesOption() =
    option(
        "-ce", "--counterexamples",
        help = "File with counter-examples",
        metavar = "<path>"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    ).convert {
        Counterexample.from(it)
    }

private const val outDirFormat = "yyyy-MM-dd_HH-mm-ss"

fun ParameterHolder.outDirOption() =
    option(
        "-o", "--outdir",
        help = "Output directory",
        metavar = "<path>"
    ).file().defaultLazy(defaultForHelp = "out/<$outDirFormat>") {
        val now = DateTime.nowLocal().format(outDirFormat)
        File("out/$now")
    }

fun ParameterHolder.smvDirOption() =
    option(
        "--smvdir",
        help = "Directory with SMV files",
        metavar = "<path>"
    ).file(
        mustExist = true,
        canBeFile = false
    ).default(
        File("data/pnp/smv")
    )

fun ParameterHolder.inputNamesOption() =
    option(
        "--input-names",
        help = "File with input variables names",
        metavar = "<path>"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    ).convert {
        it.readLines()
    }.default(
        inputNamesPnP,
        defaultForHelp = "PnP names"
    )

fun ParameterHolder.outputNamesOption() =
    option(
        "--output-names",
        help = "File with output variables names",
        metavar = "<path>"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    ).convert {
        it.readLines()
    }.default(
        outputNamesPnP,
        defaultForHelp = "PnP names"
    )
