package ru.ifmo.fbsat.core.utils

import kotlin.properties.Delegates

enum class SolverBackend {
    DEFAULT, INCREMENTAL;
}

enum class EpsilonOutputEvents {
    START, ONLYSTART, NONE;
}

enum class StartStateAlgorithms {
    NOTHING, ZERO, ZERONOTHING, ANY;
}

object Globals {
    var EPSILON_OUTPUT_EVENTS: EpsilonOutputEvents by Delegates.notNull()
    var START_STATE_ALGORITHMS: StartStateAlgorithms by Delegates.notNull()
    var IS_FORBID_OR: Boolean by Delegates.notNull()
    var IS_FORBID_TRANSITIONS_TO_FIRST_STATE: Boolean by Delegates.notNull()
    var IS_BFS_AUTOMATON: Boolean by Delegates.notNull()
    var IS_BFS_GUARD: Boolean by Delegates.notNull()
    var IS_ENCODE_TRANSITIONS_ORDER: Boolean by Delegates.notNull()
    var IS_ENCODE_TERMINALS_ORDER: Boolean by Delegates.notNull()
    var IS_ENCODE_TERMINALS_MINI_ORDER: Boolean by Delegates.notNull()
    var IS_ENCODE_TOTALIZER: Boolean by Delegates.notNull()
    var IS_DEBUG: Boolean by Delegates.notNull()
}
