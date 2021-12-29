package ru.ifmo.fbsat.core.task.extra.schema.stateful

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.ArbitraryModularAutomaton
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.generateRandomScenario
import ru.ifmo.fbsat.core.automaton.guard.BooleanExpressionGuard
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.Pins
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.boolexpr.BooleanExpression
import ru.ifmo.fbsat.core.utils.boolexpr.not
import ru.ifmo.fbsat.core.utils.multiArrayOf
import ru.ifmo.fbsat.core.utils.timeSince
import kotlin.random.Random

private val logger = MyLogger {}

internal val counterAutomaton: Automaton by lazy {
    val inputNames = listOf("input")
    val outputNames = listOf("value", "carry")
    val automaton = Automaton(
        inputEvents = listOf(InputEvent("REQ")),
        outputEvents = listOf(OutputEvent("CNF")),
        inputNames = inputNames,
        outputNames = outputNames,
        initialOutputValues = OutputValues.zeros(outputNames.size)
    )
    automaton.addState(1, OutputEvent("CNF"), BinaryAlgorithm("00", "00"))
    automaton.addState(2, OutputEvent("CNF"), BinaryAlgorithm("10", "10"))
    automaton.addState(3, OutputEvent("CNF"), BinaryAlgorithm("01", "01"))
    val exprVarInput = BooleanExpression.variable(0, inputNames[0])
    val guardInput0 = BooleanExpressionGuard(exprVarInput.not())
    val guardInput1 = BooleanExpressionGuard(exprVarInput)
    automaton.addTransition(1, 2, InputEvent("REQ"), guardInput1)
    automaton.addTransition(2, 3, InputEvent("REQ"), guardInput1)
    automaton.addTransition(3, 2, InputEvent("REQ"), guardInput1)
    automaton.addTransition(3, 1, InputEvent("REQ"), guardInput0)
    // null-transitions
    automaton.addTransition(1, 1, InputEvent("REQ"), guardInput0)
    automaton.addTransition(2, 2, InputEvent("REQ"), guardInput0)
    automaton
}

private fun runCounter() {
    Globals.IS_DEBUG = true

    val inputNames = listOf("input")
    val outputNames = listOf("out0", "out1", "out2")

    val X = inputNames.size
    val Z = outputNames.size

    val modules = multiArrayOf(counterAutomaton, counterAutomaton, counterAutomaton)
    val M = modules.shape.single()
    // val inboundVarPinParent = multiArrayOf(7, 2, 4, 1, 3, 5)
    // val inboundVarPinParent = multiArrayOf(10, 2, 5, 1, 4, 7)
    val inboundVarPinParent = with(Pins(M, X, Z, 1, 1)) {
        multiArrayOf(
            externalOutboundVarPins[0],
            modularOutboundVarPins[1][1],
            modularOutboundVarPins[2][1],
            modularOutboundVarPins[1][0],
            modularOutboundVarPins[2][0],
            modularOutboundVarPins[3][0],
        )
    }
    logger.info("inboundVarPinParent = $inboundVarPinParent")
    check(multiArrayOf(10, 2, 5, 1, 4, 7).values == inboundVarPinParent.values)
    check(inboundVarPinParent.shape.single() == 6)

    val modularAutomaton = ArbitraryModularAutomaton(
        modules,
        inboundVarPinParent,
        inputEvents = listOf(InputEvent("REQ")),
        outputEvents = listOf(OutputEvent("CNF")),
        inputNames = inputNames,
        outputNames = outputNames,
    )

    val random = Random(42)
    val numberOfScenarios = 20
    val scenarioLength = 50
    val scenarios = List(numberOfScenarios) { modularAutomaton.generateRandomScenario(scenarioLength, random) }
    val tree = PositiveScenarioTree.fromScenarios(scenarios, inputNames, outputNames)
    logger.info("Tree size: ${tree.size}")
    logger.info("Tree input names: ${tree.inputNames}")
    logger.info("Tree output names: ${tree.outputNames}")

    val blockTypes = listOf(
        GateBlock.AND,
        GateBlock.OR,
        // GateBlock.XOR,
        GateBlock.NOT,
        // GateBlock.HA,
        AutomatonBlock.CNT,
    )

    val timeStart = PerformanceCounter.reference
    // val M = 1
    if (synthesizeStatefulSystem(tree, M = M, X = X, Z = Z, blockTypes)) {
        logger.info("M = $M: SAT after %.2f s!".format(timeSince(timeStart).seconds))
    } else {
        logger.info("M = $M: UNSAT after %.2f s.".format(timeSince(timeStart).seconds))
    }

    // synthesizeStatefulSystemIterativelyBottomUp(tree, X = X, Z = Z, minM = 1, maxM = 10, blockTypes)
}

private fun main() {
    val timeStart = PerformanceCounter.reference

    runCounter()

    logger.info("All done in %.3f s.".format(timeSince(timeStart).seconds))
}
