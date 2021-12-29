package ru.ifmo.fbsat.core.automaton

import ru.ifmo.fbsat.core.automaton.guard.BooleanExpressionGuard
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.boolexpr.randomBooleanExpression
import ru.ifmo.fbsat.core.utils.randomBinaryString
import ru.ifmo.fbsat.core.utils.toBooleanArray
import kotlin.random.Random

private val logger = MyLogger {}

fun newRandomAutomaton(
    C: Int,
    P: Int,
    I: Int,
    O: Int,
    X: Int,
    Z: Int,
    random: Random = Random,
): Automaton {
    logger.debug { "Generating random automaton with C=$C, P=$P, I=$I, O=$O, X=$X, Z=$Z..." }
    val inputEvents = if (I == 1) {
        listOf(InputEvent("REQ"))
    } else {
        (1..I).map { InputEvent("I$it") }
    }
    val outputEvents = if (O == 1) {
        listOf(OutputEvent("CNF"))
    } else {
        (1..O).map { OutputEvent("O$it") }
    }
    val inputNames = if (X == 1) {
        listOf("x")
    } else {
        (1..X).map { "x$it" }
    }
    val outputNames = if (Z == 1) {
        listOf("z")
    } else {
        (1..Z).map { "z$it" }
    }
    val initialOutputValues = OutputValues.zeros(Z)
    val automaton = Automaton(inputEvents, outputEvents, inputNames, outputNames, initialOutputValues)

    for (c in 1..C) {
        val outputEvent = outputEvents.random(random)
        val algorithm = BinaryAlgorithm(
            randomBinaryString(Z, random = random).toBooleanArray(),
            randomBinaryString(Z, random = random).toBooleanArray()
        )
        automaton.addState(c, outputEvent, algorithm)
    }

    for (a in 1..C) {
        for (b in 1..C) {
            // No loops
            if (a == b) continue

            val inputEvent = inputEvents.random(random)
            // val allInputs = (0 until 2.pow(X)).map { f ->
            //     InputValues(f.toString(2).padStart(X, '0'))
            // }
            // val truthTable: Map<InputValues, Boolean> = allInputs.associateWith {
            //     random.nextBoolean()
            // }
            // val guard = TruthTableGuard(truthTable)
            val expr = randomBooleanExpression(P, inputNames, random)
            val guard = BooleanExpressionGuard(expr)
            automaton.addTransition(a, b, inputEvent, guard)
        }
    }

    return automaton
}

fun Automaton.randomWalk(random: Random = Random): Sequence<Automaton.EvalResult> {
    val inputActions = sequence {
        while (true) {
            val action = InputAction(
                event = inputEvents.random(random),
                values = InputValues(randomBinaryString(inputNames.size, random = random))
            )
            yield(action)
        }
    }
    return eval(inputActions)
}

fun Automaton.generateRandomScenario(length: Int, random: Random = Random): PositiveScenario {
    // logger.debug { "Generating random scenario of length $length..." }
    val elements = randomWalk(random)
        .take(length)
        .map {
            ScenarioElement(
                inputAction = it.inputAction,
                outputAction = it.outputAction
            )
        }
    return PositiveScenario(elements.toList())
}

fun ArbitraryModularAutomaton.randomWalk(random: Random = Random): Sequence<ArbitraryModularAutomaton.EvalResult> {
    val inputActions = sequence {
        while (true) {
            val action = InputAction(
                event = inputEvents.random(random),
                values = InputValues(randomBinaryString(inputNames.size, random = random))
            )
            yield(action)
        }
    }
    return eval(inputActions)
}

fun ArbitraryModularAutomaton.generateRandomScenario(length: Int, random: Random = Random): PositiveScenario {
    // logger.debug { "Generating random scenario of length $length..." }
    val elements = randomWalk(random)
        .take(length)
        .map {
            ScenarioElement(
                inputAction = it.inputAction,
                outputAction = it.outputAction
            )
        }
    return PositiveScenario(elements.toList())
}
