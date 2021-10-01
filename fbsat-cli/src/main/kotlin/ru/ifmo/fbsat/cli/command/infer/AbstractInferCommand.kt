package ru.ifmo.fbsat.cli.command.infer

import com.github.ajalt.clikt.core.CliktCommand
import ru.ifmo.fbsat.cli.command.infer.options.ExtraOptions
import ru.ifmo.fbsat.cli.command.infer.options.SolverOptions
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger

private val logger = MyLogger {}

abstract class AbstractInferCommand<AutomatonType : Any>(name: String) : CliktCommand(name = name) {
    protected abstract val solverOptions: SolverOptions
    protected abstract val extraOptions: ExtraOptions

    final override fun run() {
        logger.info("Command: $commandHelp (${this::class.java.simpleName})")
        setupGlobals()
        setup()
        val automaton: AutomatonType? = infer()
        printAndCheck(automaton)
    }

    private fun setupGlobals() {
        with(extraOptions) {
            Globals.EPSILON_OUTPUT_EVENTS = epsilonOutputEvents
            Globals.START_STATE_ALGORITHMS = startStateAlgorithms
            Globals.IS_FORBID_OR = isForbidOr
            Globals.IS_FORBID_TRANSITIONS_TO_FIRST_STATE = isForbidTransitionsToFirstState
            Globals.IS_BFS_AUTOMATON = isBfsAutomaton
            Globals.IS_BFS_GUARD = isBfsGuard
            Globals.IS_ENCODE_REVERSE_IMPLICATION = isEncodeReverseImplication
            Globals.IS_ENCODE_TRANSITIONS_ORDER = isEncodeTransitionsOrder
            Globals.IS_ENCODE_TERMINALS_ORDER = isEncodeTerminalsOrder
            Globals.IS_ENCODE_TERMINALS_MINI_ORDER = isEncodeTerminalsMiniOrder
            Globals.IS_ENCODE_HARD_TO_EXPLAIN = isEncodeHardToExplain
            Globals.IS_ENCODE_TOTALIZER = isEncodeTotalizer
            Globals.IS_ENCODE_DISJUNCTIVE_TRANSITIONS = isEncodeDisjunctiveTransitions
            Globals.IS_ENCODE_ATF_0 = isEncodeAtf0
            Globals.IS_ENCODE_FF_0_VARDECL = isEncodeFf0VarDecl
            Globals.IS_ENCODE_FF_NF_VARDECL = isEncodeFfNfVarDecl
            Globals.IS_ENCODE_ACTIVE_PASSIVE = isEncodeActivePassive
            Globals.IS_ENCODE_TRANSITION_FUNCTION = isEncodeTransitionFunction
            Globals.IS_ENCODE_EPSILON_PASSIVE = isEncodeEpsilonPassive
            Globals.IS_ENCODE_NOT_EPSILON_ACTIVE = isEncodeNotEpsilonActive
            Globals.IS_REUSE_K = isReuseK
            Globals.IS_USE_ASSUMPTIONS = isUseAssumptions
            Globals.IS_RENDER_WITH_DOT = isRenderWithDot
            Globals.IS_DUMP_CNF = isDumpCnf
            Globals.IS_DUMP_VARS_IN_CNF = isDumpVarsInCnf
            Globals.IS_DEBUG = isDebug
        }
    }

    protected abstract fun setup()
    protected abstract fun infer(): AutomatonType?
    protected abstract fun printAndCheck(automaton: AutomatonType?)
}
