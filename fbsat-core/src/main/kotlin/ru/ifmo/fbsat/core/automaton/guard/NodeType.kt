package ru.ifmo.fbsat.core.automaton.guard

enum class NodeType(val value: Int) {
    TERMINAL(1),
    AND(2),
    OR(3),
    NOT(4),
    NONE(5);

    companion object {
        private val lookup: Map<Int, NodeType> = values().associateBy(NodeType::value)

        fun from(value: Int): NodeType = lookup.getValue(value)
    }
}
