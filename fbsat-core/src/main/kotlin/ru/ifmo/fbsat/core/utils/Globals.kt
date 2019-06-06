package ru.ifmo.fbsat.core.utils

import kotlin.properties.Delegates

object Globals {
    var IS_FORBID_OR: Boolean by Delegates.notNull()
    var IS_BFS_AUTOMATON: Boolean by Delegates.notNull()
    var IS_BFS_GUARD: Boolean by Delegates.notNull()
    var IS_ENCODE_TRANSITIONS_ORDER: Boolean by Delegates.notNull()
    var IS_ENCODE_TERMINALS_ORDER: Boolean by Delegates.notNull()
    var IS_ENCODE_TOTALIZER: Boolean by Delegates.notNull()
    var IS_DEBUG: Boolean by Delegates.notNull()
}
