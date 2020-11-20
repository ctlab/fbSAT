package ru.ifmo.fbsat.core.utils

private fun List<String>.joinToStringAutoBrace(separator: String): String {
    val s = joinToString(separator)
    return if (size > 1) "($s)" else s
}

fun makeDnfString(
    terms: List<List<String>>,
    conjunction: String = "&",
    disjunction: String = "|",
): String =
    if (terms.isEmpty()) "0"
    else terms.joinToString(disjunction) { it.joinToString(conjunction) }

fun makeCnfString(
    terms: List<List<String>>,
    conjunction: String = "&",
    disjunction: String = "|",
): String =
    if (terms.isEmpty()) "1"
    else terms.joinToString(conjunction) { it.joinToStringAutoBrace(disjunction) }
