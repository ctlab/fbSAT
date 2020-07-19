@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.options

import com.github.ajalt.clikt.core.ParameterHolder
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.types.int

internal const val AUTOMATON_OPTIONS = "Automaton Options"

@Deprecated("This is just a template with ALL options. Create your own class.")
class AutomatonOptions private constructor() : OptionGroup(AUTOMATON_OPTIONS) {
    val numberOfModules: Int? by numberOfModulesOption()
    val numberOfStates: Int? by numberOfStatesOption()
    val maxOutgoingTransitions: Int? by maxOutgoingTransitionsOption()
    val maxGuardSize: Int? by maxGuardSizeOption()
    val maxTransitions: Int? by maxTransitionsOption()
    val maxTotalGuardsSize: Int? by maxTotalGuardsSizeOption()
    val maxPlateauWidth: Int? by maxPlateauWidthOption()
}

fun ParameterHolder.numberOfStatesOption() =
    option(
        "-C",
        help = "Number of automaton states",
        metavar = "<int>"
    ).int()

fun ParameterHolder.maxOutgoingTransitionsOption() =
    option(
        "-K",
        help = "Maximum number of transitions from each state",
        metavar = "<int>"
    ).int()

fun ParameterHolder.maxGuardSizeOption() =
    option(
        "-P",
        help = "Maximum guard size (number of parse tree nodes)",
        metavar = "<int>"
    ).int()

fun ParameterHolder.maxTransitionsOption() =
    option(
        "-T",
        help = "Upper bound for the total number of transitions",
        metavar = "<int>"
    ).int()

fun ParameterHolder.maxTotalGuardsSizeOption() =
    option(
        "-N",
        help = "Upper bound for the total size of guards",
        metavar = "<int>"
    ).int()

fun ParameterHolder.maxPlateauWidthOption() =
    option(
        "-w",
        help = "Maximum plateau width",
        metavar = "<int>"
    ).int()

fun ParameterHolder.numberOfModulesOption() =
    option(
        "-M",
        help = "Number of modules",
        metavar = "<int>"
    ).int()

fun ParameterHolder.startNumberOfStatesOption() =
    option(
        "-Cstart",
        help = "Start number of automaton states",
        metavar = "<int>"
    ).int()

fun ParameterHolder.endNumberOfStatesOption() =
    option(
        "-Cend",
        help = "End number of automaton states",
        metavar = "<int>"
    ).int()

fun ParameterHolder.startMaxGuardSizeOption() =
    option(
        "-Pstart",
        help = "Start max guard size",
        metavar = "<int>"
    ).int()

fun ParameterHolder.endMaxGuardSizeOption() =
    option(
        "-Pend",
        help = "End max guard size",
        metavar = "<int>"
    ).int()
