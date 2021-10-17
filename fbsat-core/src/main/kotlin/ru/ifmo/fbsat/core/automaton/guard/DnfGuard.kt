package ru.ifmo.fbsat.core.automaton.guard

import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.utils.makeDnfString
import kotlin.math.absoluteValue

class DnfGuard(
    val dnf: List<List<String>>,
    val inputNames: List<String>,
) : Guard {
    // Note: `_dnf` is one-based and signed
    private val _dnf: List<List<Int>> =
        dnf.map { term ->
            term.map { s ->
                if (s.startsWith('!') || s.startsWith('~')) {
                    -(inputNames.indexOf(s.drop(1)) + 1)
                } else {
                    inputNames.indexOf(s) + 1
                }
            }
        }

    override val size: Int = dnf.sumOf { it.size }

    override fun eval(inputValues: InputValues): Boolean =
        if (_dnf.isEmpty()) {
            check(toSimpleString() == "0")
            false
        } else {
            _dnf.any { term ->
                term.all { Lit ->
                    (Lit < 0) xor inputValues.values[Lit.absoluteValue - 1]
                }
            }
        }

    override fun toSimpleString(): String {
        return makeDnfString(dnf, conjunction = "&", disjunction = " | ")
    }

    override fun toGraphvizString(): String {
        return toSimpleString()
    }

    override fun toFbtString(): String {
        return toSimpleString()
            .replace("|", " OR ")
            .replace("&", " AND ")
            .replace("~", "NOT ")
            .replace("!", "NOT ")
    }

    override fun toSmvString(): String {
        return toSimpleString()
    }

    override fun toString(): String {
        return "DnfGuard(dnf = $dnf)"
    }
}
