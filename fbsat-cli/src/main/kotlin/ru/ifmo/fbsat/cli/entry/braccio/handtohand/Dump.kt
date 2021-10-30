@file:Suppress("DuplicatedCode")

package ru.ifmo.fbsat.cli.entry.braccio.handtohand

import com.soywiz.klock.DateTime
import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.guard.Guard
import ru.ifmo.fbsat.core.automaton.newRandomAutomaton
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.timeSince
import kotlin.random.Random

private val logger = MyLogger {}

fun Automaton.toHandToHandControllerCppString(
    controllerName: String, // "ControllerX"
    includeGuardName: String = controllerName.uppercase() + "_HPP",
    outputNamesBoolean: List<String>,
    outputNamesIntWithDomain: List<Pair<String, List<Int>>>,
): String {
    require(!controllerName.contains(" ")) { "Name should not contain spaces" }

    fun String.myIndent(): String =
        lineSequence()
            .map {
                if (it.isBlank()) {
                    ""
                } else {
                    " ".repeat(4) + it
                }
            }
            .joinToString("\n")

    fun List<String>.myIndent(): List<String> = map { it.myIndent() }

    fun String.toEnumName(): String {
        return "Val" +
            this.split("_")
                .joinToString("") {
                    it.replaceFirstChar { c ->
                        if (c.isLowerCase()) c.titlecase() else c.toString()
                    }
                }
    }

    val angleEnumsLines: MutableList<String> = mutableListOf()
    for ((name, domain) in outputNamesIntWithDomain) {
        angleEnumsLines += "enum class ${name.toEnumName()} {"
        for ((i, v) in domain.withIndex()) {
            val postfix = if (i != domain.lastIndex) "," else ""
            angleEnumsLines += "V${v} = $v$postfix".myIndent()
        }
        angleEnumsLines += "};"
    }

    fun Automaton.State.name(): String {
        return "S${id}"
    }

    val stateNames = states.map { it.name() }
    val allStateNames = stateNames + "__SAME__"
    val stateEnumLines: MutableList<String> = mutableListOf()
    stateEnumLines += "enum State {"
    for ((i, name) in allStateNames.withIndex()) {
        val postfix = if (i != allStateNames.lastIndex) "," else ""
        stateEnumLines += "$name$postfix".myIndent()
    }
    stateEnumLines += "} state = ${initialState.name()};"

    val inputStructOperatorEqFunctionLines: MutableList<String> = mutableListOf()
    inputStructOperatorEqFunctionLines += "bool operator==(const Input& other) const {"
    inputStructOperatorEqFunctionLines += inputNames.withIndex().map { (i, inputName) ->
        val prefix = if (i == 0) "return " else "       "
        val postfix = if (i == inputNames.lastIndex) ";" else " &&"
        "$prefix$inputName == other.$inputName$postfix"
    }.myIndent()
    inputStructOperatorEqFunctionLines += "}"

    val inputStructLines: MutableList<String> = mutableListOf()
    inputStructLines += "struct Input {"
    for (inputName in inputNames) {
        inputStructLines += "bool $inputName;".myIndent()
    }

    inputStructLines += ""
    inputStructLines += inputStructOperatorEqFunctionLines.myIndent()
    inputStructLines += "};"

    val outStructLines: MutableList<String> = mutableListOf()
    outStructLines += "struct Out {"
    for (name in outputNamesBoolean) {
        outStructLines += "bool $name;".myIndent()
    }
    for ((name, _) in outputNamesIntWithDomain) {
        outStructLines += "${name.toEnumName()} $name;".myIndent()
    }
    outStructLines += "} out;"

    val state2stringFunctionLines: MutableList<String> = mutableListOf()
    state2stringFunctionLines += "String state2string(State s) {"
    val switchLines: MutableList<String> = mutableListOf()
    switchLines += "switch (s) {"
    for (s in allStateNames) {
        switchLines += "case $s:".myIndent()
        switchLines += "return \"$s\";".myIndent().myIndent()
    }
    switchLines += "default:".myIndent()
    switchLines += "return \"\";".myIndent().myIndent()
    switchLines += "}"
    state2stringFunctionLines += switchLines.myIndent()
    state2stringFunctionLines += "}"

    fun Guard.toCString(): String =
        toSmvString()
            .replace("&", "&&")
            .replace("|", "||")

    val goStepLines: MutableList<String> = mutableListOf()
    for (inputName in inputNames) {
        goStepLines += "bool $inputName = input.$inputName;"
    }
    goStepLines += ""
    goStepLines += "State next_state = __SAME__;"
    goStepLines += ""
    goStepLines += "// Compute the next state"
    goStepLines += "if (0) {"
    for (s in states) {
        for (t in s.transitions) {
            goStepLines += "} else if (state == ${s.name()} && (${t.guard.toCString()})) {"
            goStepLines += "next_state = ${t.destination.name()};".myIndent()
        }
    }
    goStepLines += "}"
    goStepLines += ""
    goStepLines += "// Compute outputs"
    goStepLines += "if (0) {"
    for (s in states) {
        val algo = s.algorithm as BinaryAlgorithm
        goStepLines += "} else if (next_state == ${s.name()}) {"
        for (name in outputNamesBoolean) {
            val z = outputNames.indexOf(name) + 1
            val newValueBot = algo.algorithm0[z - 1]
            val newValueTop = algo.algorithm1[z - 1]
            if (newValueBot == newValueTop) {
                // const algo
                goStepLines += "out.$name = $newValueBot;".myIndent()
            } else if (newValueBot) {
                // flip algo
                goStepLines += "out.$name = !out.$name;".myIndent()
            } else {
                // noop algo
            }
        }
        for ((name, domain) in outputNamesIntWithDomain) {
            // Note: assuming const algorithms!
            check(algo.algorithm0.contentEquals(algo.algorithm1)) {
                "Non-const algorithms are not supported"
            }
            val v = domain.firstOrNull { v ->
                val z = outputNames.indexOf("${name}_$v") + 1
                algo.algorithm0[z - 1]
            }
            if (v != null) {
                goStepLines += "out.$name = ${name.toEnumName()}::V$v;".myIndent()
            }
        }
    }
    goStepLines += "}"
    goStepLines += ""
    goStepLines += """
        // Update the state
        if (next_state != __SAME__) {
            state = next_state;
        }

        // return is_active
        return (next_state != __SAME__);
    """.trimIndent().split("\n")

    val goStepFunctionLines: MutableList<String> = mutableListOf()
    goStepFunctionLines += "bool go_step(Input input) {"
    goStepFunctionLines += goStepLines.myIndent()
    goStepFunctionLines += "}"

    val structLines: MutableList<String> = mutableListOf()
    structLines += "struct $controllerName {"
    structLines += angleEnumsLines.myIndent()
    structLines += ""
    structLines += stateEnumLines.myIndent()
    structLines += ""
    structLines += inputStructLines.myIndent()
    structLines += ""
    structLines += outStructLines.myIndent()
    structLines += ""
    structLines += state2stringFunctionLines.myIndent()
    structLines += ""
    structLines += goStepFunctionLines.myIndent()
    structLines += "};"

    val codeLines: MutableList<String> = mutableListOf()
    codeLines += "#ifndef $includeGuardName"
    codeLines += "#define $includeGuardName"
    codeLines += ""
    codeLines += structLines
    codeLines += ""
    codeLines += "#endif  // $includeGuardName"

    return codeLines.joinToString("\n")
}

fun main() {
    val dateTimeFormat = "yyyy-MM-dd HH:mm:ss"
    logger.info("Start time: ${DateTime.nowLocal().format(dateTimeFormat)}")
    val timeStart = PerformanceCounter.reference

    val random = Random(42)
    val automaton = newRandomAutomaton(
        C = 3,
        P = 3,
        I = 1,
        O = 1,
        X = 4,
        Z = 2,
        random = random
    )
    println("Automaton:")
    automaton.pprint()

    println("Arduino-code:")
    println(automaton.toHandToHandControllerCppString(
        controllerName = "Controller",
        outputNamesBoolean = automaton.outputNames,
        outputNamesIntWithDomain = emptyList()
    ))

    logger.info("End time: ${DateTime.nowLocal().format(dateTimeFormat)}")
    logger.info("All done in %.3f seconds".format(timeSince(timeStart).seconds))
}
