@file:Suppress("PublicApiImplicitType")

package ru.ifmo.fbsat.cli.command.infer.options

import com.github.ajalt.clikt.core.ParameterHolder
import com.github.ajalt.clikt.parameters.groups.OptionGroup
import com.github.ajalt.clikt.parameters.options.convert
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.types.choice
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.utils.EpsilonOutputEvents
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.NegativeTreeOptimizations
import ru.ifmo.fbsat.core.utils.StartStateAlgorithms

internal const val EXTRA_OPTIONS = "Extra Options"

@Suppress("MemberVisibilityCanBePrivate")
class ExtraOptions : OptionGroup(EXTRA_OPTIONS) {
    val isForbidOr: Boolean by isForbidOrOption()
    val isForbidTransitionsToFirstState: Boolean by isForbidTransitionsToFirstStateOption()
    val isBfsAutomaton: Boolean by isBfsAutomatonOption()
    val isBfsGuard: Boolean by isBfsGuardOption()

    // val isOnlyC: Boolean by isOnlyCOption()
    // val failIfSTVerifyFailed: Boolean by getFailIfSTVerifyFailedOption()
    val initialOutputValues: OutputValues? by getInitialOutputValuesOption()
    val epsilonOutputEvents: EpsilonOutputEvents by getEpsilonOutputEventsOption()
    val startStateAlgorithms: StartStateAlgorithms by getStartStateAlgorithmsOption()
    val isEncodeReverseImplication: Boolean by isEncodeReverseImplicationOption()
    val isEncodeTransitionsOrder: Boolean by isEncodeTransitionsOrderOption()
    val isEncodeTerminalsOrder: Boolean by isEncodeTerminalsOrderOption()
    val isEncodeTerminalsMiniOrder: Boolean by isEncodeTerminalsMiniOrderOption()
    val isEncodeHardToExplain: Boolean by isEncodeHardToExplainOption()
    val isEncodeTotalizer: Boolean by isEncodeTotalizerOption()
    val isEncodeDisjunctiveTransitions: Boolean by isEncodeDisjunctiveTransitionsOption()
    val isReuseK: Boolean by isReuseKOption()
    val isUseAssumptions: Boolean by isUseAssumptionsOption()
    val isDumpVarsInCnf: Boolean by isDumpVarsInCnfOption()
    val isDebug: Boolean by isDebugOption()
    val negativeTreeOptimizations: NegativeTreeOptimizations by negativeTreeOptimizationsOption()
    val looplessCounterExamples: Boolean by looplessCounterExamples()
}

fun ParameterHolder.isForbidOrOption() =
    option(
        "--forbid-or"
    ).flag(
        "--allow-or",
        default = Globals.IS_FORBID_OR
    )

fun ParameterHolder.isForbidTransitionsToFirstStateOption() =
    option(
        "--forbid-transitions-to-first-state"
    ).flag(
        "--allow-transitions-to-first-state",
        default = Globals.IS_FORBID_TRANSITIONS_TO_FIRST_STATE
    )

fun ParameterHolder.isBfsAutomatonOption() =
    option(
        "--bfs-automaton"
    ).flag(
        "--no-bfs-automaton",
        default = Globals.IS_BFS_AUTOMATON
    )

fun ParameterHolder.isBfsGuardOption() =
    option(
        "--bfs-guard"
    ).flag(
        "--no-bfs-guard",
        default = Globals.IS_BFS_GUARD
    )

fun ParameterHolder.isOnlyCOption() =
    option(
        "--only-C",
        help = "[basic-min] Minimize only C, without T"
    ).flag()

fun ParameterHolder.getFailIfSTVerifyFailedOption() =
    option(
        "--fail-verify-st",
        help = "Halt if verification of scenario tree has failed"
    ).flag(
        "--no-fail-verify-st",
        default = true
    )

fun ParameterHolder.getInitialOutputValuesOption() =
    option(
        "--initial-output-values",
        help = "Initial output values (as a bitstring)",
        metavar = "<[01]+>"
    ).convert {
        require(it.matches(Regex("[01]+"))) {
            "--initial-output-values must match [01]+"
        }
        OutputValues(it.map { c -> c == '1' })
    }

fun ParameterHolder.getEpsilonOutputEventsOption() =
    option(
        "--epsilon-output-events",
        help = "Epsilon output events"
    ).choice(
        "start" to EpsilonOutputEvents.START,
        "onlystart" to EpsilonOutputEvents.ONLYSTART,
        "none" to EpsilonOutputEvents.NONE
    ).default(
        Globals.EPSILON_OUTPUT_EVENTS
    )

fun ParameterHolder.getStartStateAlgorithmsOption() =
    option(
        "--start-state-algorithms",
        help = "Start state algorithms"
    ).choice(
        "nothing" to StartStateAlgorithms.NOTHING,
        "zero" to StartStateAlgorithms.ZERO,
        "zeronothing" to StartStateAlgorithms.ZERONOTHING,
        "any" to StartStateAlgorithms.ANY,
        "init" to StartStateAlgorithms.INIT,
        "initnothing" to StartStateAlgorithms.INITNOTHING
    ).default(
        Globals.START_STATE_ALGORITHMS
    )

fun ParameterHolder.isEncodeReverseImplicationOption() =
    option(
        "--encode-reverse-implication",
        help = "Encode reverse implication"
    ).flag(
        "--no-encode-reverse-implication",
        default = true
    )

fun ParameterHolder.isEncodeTransitionsOrderOption() =
    option(
        "--encode-transitions-order",
        help = "[DEBUG] Encode transitions lexicographic order"
    ).flag(
        "--no-encode-transitions-order",
        default = Globals.IS_ENCODE_TRANSITIONS_ORDER
    )

fun ParameterHolder.isEncodeTerminalsOrderOption() =
    option(
        "--encode-terminals-order",
        help = "[DEBUG] Encode terminal numbers lexicographic order"
    ).flag(
        "--no-encode-terminals-order",
        default = Globals.IS_ENCODE_TERMINALS_ORDER
    )

fun ParameterHolder.isEncodeTerminalsMiniOrderOption() =
    option(
        "--encode-terminals-mini-order",
        help = "[DEBUG] Encode AND/OR children-terminals order"
    ).flag(
        "--no-encode-terminals-mini-order",
        default = Globals.IS_ENCODE_TERMINALS_MINI_ORDER
    )

fun ParameterHolder.isEncodeHardToExplainOption() =
    option(
        "--encode-hard-to-explain",
        help = "[DEBUG] Encode some hard to explain thing"
    ).flag(
        "--no-encode-hard-to-explain",
        default = Globals.IS_ENCODE_HARD_TO_EXPLAIN
    )

fun ParameterHolder.isEncodeTotalizerOption() =
    option(
        "--encode-totalizer",
        help = "Encode totalizer when upper bound is null"
    ).flag(
        "--no-encode-totalizer",
        default = Globals.IS_ENCODE_TOTALIZER
    )

fun ParameterHolder.isEncodeDisjunctiveTransitionsOption() =
    option(
        "--encode-disjunctive-transitions",
        help = "Encode disjunctive transitions (adhocly forbid priority function)"
    ).flag(
        "--no-encode-disjunctive-transitions",
        default = Globals.IS_ENCODE_DISJUNCTIVE_TRANSITIONS
    )

fun ParameterHolder.isReuseKOption() =
    option(
        "--reuse-k",
        help = "Reuse K found by ExtendedMinTask during CEGIS"
    ).flag(
        "--no-reuse-k",
        default = Globals.IS_REUSE_K
    )

fun ParameterHolder.isUseAssumptionsOption() =
    option(
        "--use-assumptions",
        help = "Use assumptions during cardinality optimizations"
    ).flag(
        "--no-use-assumptions",
        default = Globals.IS_USE_ASSUMPTIONS
    )

fun ParameterHolder.isDumpVarsInCnfOption() =
    option(
        "--dump-vars-in-cnf",
        help = "Dump variables in CNF"
    ).flag(
        "--no-dump-vars-in-cnf",
        default = Globals.IS_DUMP_VARS_IN_CNF
    )

fun ParameterHolder.isDebugOption() =
    option(
        "--debug",
        help = "Debug mode"
    ).flag(
        "--no-debug",
        default = Globals.IS_DEBUG
    )

fun ParameterHolder.negativeTreeOptimizationsOption() =
    option(
        "--negative-tree-optimizations",
        help = "Negative tree optimizations"
    ).choice(
        "nothing" to NegativeTreeOptimizations.NOTHING,
        "opt1" to NegativeTreeOptimizations.OPT1,
        "opt2" to NegativeTreeOptimizations.OPT2,
    ).default(
        NegativeTreeOptimizations.NOTHING
    )

fun ParameterHolder.looplessCounterExamples() =
    option(
        "--loopless-counter-examples",
        help = "Allow loopless counter-examples"
    ).flag(
        "--no-loopless-counter-examples",
        default = Globals.LOOPLESS_COUNTER_EXAMPLES
    )
