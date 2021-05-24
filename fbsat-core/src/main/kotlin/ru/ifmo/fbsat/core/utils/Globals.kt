package ru.ifmo.fbsat.core.utils

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.satlib.solver.GlucoseSolver
import com.github.lipen.satlib.solver.MiniSatSolver
import org.redundent.kotlin.xml.PrintOptions
import ru.ifmo.fbsat.core.scenario.OutputValues
import kotlin.properties.Delegates

enum class EpsilonOutputEvents {
    START, ONLYSTART, NONE;
}

enum class StartStateAlgorithms {
    NOTHING, ZERO, ZERONOTHING, ANY, INIT, INITNOTHING;
}

enum class NegativeTreeOptimizations {
    NOTHING, OPT1, OPT2
}

object Globals {
    var INITIAL_OUTPUT_VALUES: OutputValues by Delegates.notNull()
    var EPSILON_OUTPUT_EVENTS: EpsilonOutputEvents = EpsilonOutputEvents.ONLYSTART
    var START_STATE_ALGORITHMS: StartStateAlgorithms = StartStateAlgorithms.ZERO
    var IS_FORBID_OR: Boolean = false
    var IS_FORBID_TRANSITIONS_TO_FIRST_STATE: Boolean = true
    var IS_BFS_AUTOMATON: Boolean = true
    var IS_BFS_GUARD: Boolean = true
    var IS_ENCODE_TRANSITIONS_ORDER: Boolean = true
    var IS_ENCODE_TERMINALS_ORDER: Boolean = false
    var IS_ENCODE_TERMINALS_MINI_ORDER: Boolean = false
    var IS_ENCODE_HARD_TO_EXPLAIN: Boolean = true
    var IS_ENCODE_TOTALIZER: Boolean = true
    var IS_ENCODE_DISJUNCTIVE_TRANSITIONS: Boolean = false
    var IS_DEBUG: Boolean = false
    var IS_REUSE_K: Boolean = true
    var IS_DUMP_VARS_IN_CNF: Boolean = false
    var IS_USE_ASSUMPTIONS: Boolean = false
    var IS_MODULAR_ARBITRARY_EXTENDED: Boolean = false
    val xmlPrintOptions: PrintOptions =
        PrintOptions(pretty = true, singleLineTextElements = true, useSelfClosingTags = true)
    var modularName: MultiArray<String> by Delegates.notNull()
    var ICMS_CMD: String = "incremental-cryptominisat"
    var MINISAT_SIMP_STRATEGY: MiniSatSolver.Companion.SimpStrategy? = null
    var GLUCOSE_SIMP_STRATEGY: GlucoseSolver.Companion.SimpStrategy? = null
    var NEGATIVE_TREE_OPTIMIZATIONS: NegativeTreeOptimizations = NegativeTreeOptimizations.NOTHING
    var LOOPLESS_COUNTER_EXAMPLES: Boolean = false
}
