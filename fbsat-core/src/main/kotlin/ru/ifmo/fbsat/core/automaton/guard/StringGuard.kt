package ru.ifmo.fbsat.core.automaton.guard

import ru.ifmo.fbsat.core.scenario.InputValues

class StringGuard(val expr: String, val inputNames: List<String>) : Guard {
    private val literals = expr.splitToSequence('&').map(String::trim).toList()

    override val size: Int =
        2 * literals.size - 1 + literals.count { it.startsWith("!") || it.startsWith("~") }

    override fun truthTableString(inputNames: List<String>): String {
        TODO()
    }

    override fun eval(inputValues: InputValues): Boolean {
        return literals.all {
            if (it.startsWith("!") || it.startsWith("~"))
                !inputValues[inputNames.indexOf(it.substring(1))]
            else
                inputValues[inputNames.indexOf(it)]
        }
    }

    override fun toSimpleString(): String {
        return expr
    }

    override fun toGraphvizString(): String {
        return expr
    }

    override fun toFbtString(): String {
        return expr
            .replace("&", " AND ")
            .replace("~", "NOT ")
            .replace("!", "NOT ")
    }

    override fun toSmvString(): String {
        return expr
    }

    override fun toString(): String {
        return "StringGuard(expr = $expr)"
    }
}
