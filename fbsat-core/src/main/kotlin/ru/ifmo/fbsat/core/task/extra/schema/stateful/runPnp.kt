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
import ru.ifmo.fbsat.core.utils.boolexpr.Variable
import ru.ifmo.fbsat.core.utils.boolexpr.and
import ru.ifmo.fbsat.core.utils.boolexpr.not
import ru.ifmo.fbsat.core.utils.inputNamesPnP
import ru.ifmo.fbsat.core.utils.multiArrayOf
import ru.ifmo.fbsat.core.utils.outputNamesPnP
import ru.ifmo.fbsat.core.utils.timeSince
import ru.ifmo.fbsat.core.utils.toMultiArray
import kotlin.random.Random

private val logger = MyLogger {}

// fbsat infer modular-parallel-extended-min -M 3 -P 5 -i data\tests-1.gz --glucose

internal val pnpModule1: Automaton by lazy {
    val inputNames = listOf("c1Home", "c1End", "c2Home", "c2End", "vcHome", "vcEnd", "pp1", "pp2", "pp3", "vac")
    // listOf("c1Extend", "c1Retract", "c2Extend", "c2Retract", "vcExtend", "vacuum_on", "vacuum_off")
    val outputNames = listOf("vacuum_on")
    val automaton = Automaton(
        inputEvents = listOf(InputEvent("REQ")),
        outputEvents = listOf(OutputEvent("CNF")),
        inputNames = inputNames,
        outputNames = outputNames,
        initialOutputValues = OutputValues.zeros(outputNames.size)
    )

    fun getVar(name: String): Variable {
        return BooleanExpression.variable(inputNames.indexOf(name), name)
    }

    fun addTrans(src: Int, dest: Int, expr: BooleanExpression) {
        automaton.addTransition(src, dest, InputEvent("REQ"), BooleanExpressionGuard(expr))
    }

    // output-decomposition = MultiArray(shape = [7], values = [3, 3, 2, 3, 3, 1, 2])
    // [I] Module #1 (C = 4, K = 2, P = 5, T = 6, N = 12):
    // Automaton Hash-Code: 869568
    //   ┌─1/ε(0:0, 1:0)
    //   └──1 to 2 on REQ if pp1
    //   ┌─2/CNF(0:0, 1:1)
    //   ├──2 to 3 on REQ if (c1Home & (vcEnd & vac))
    //   └──2 to 4 on REQ if (vcHome & c1End)
    //   ┌─3/CNF(0:1, 1:0)
    //   └──3 to 2 on REQ if vcEnd
    //   ┌─4/CNF(0:0, 1:1)
    //   ├──4 to 3 on REQ if vcEnd
    //   └──4 to 2 on REQ if c1Home

    automaton.addState(1, OutputEvent("CNF"), BinaryAlgorithm("0", "1"))
    automaton.addState(2, OutputEvent("CNF"), BinaryAlgorithm("0", "1"))
    automaton.addState(3, OutputEvent("CNF"), BinaryAlgorithm("1", "0"))
    automaton.addState(4, OutputEvent("CNF"), BinaryAlgorithm("0", "1"))

    addTrans(1, 2, getVar("pp1"))
    addTrans(2, 3, getVar("c1Home") and getVar("vcEnd") and getVar("vac"))
    addTrans(2, 4, getVar("vcHome") and getVar("c1End"))
    addTrans(3, 2, getVar("vcEnd"))
    addTrans(4, 3, getVar("vcEnd"))
    addTrans(4, 2, getVar("c1Home"))

    automaton
}

internal val pnpModule2: Automaton by lazy {
    val inputNames = listOf("c1Home", "c1End", "c2Home", "c2End", "vcHome", "vcEnd", "pp1", "pp2", "pp3", "vac")
    // listOf("c1Extend", "c1Retract", "c2Extend", "c2Retract", "vcExtend", "vacuum_on", "vacuum_off")
    val outputNames = listOf("c2Extend", "vacuum_off")
    val automaton = Automaton(
        inputEvents = listOf(InputEvent("REQ")),
        outputEvents = listOf(OutputEvent("CNF")),
        inputNames = inputNames,
        outputNames = outputNames,
        initialOutputValues = OutputValues.zeros(outputNames.size)
    )

    fun getVar(name: String): Variable {
        return BooleanExpression.variable(inputNames.indexOf(name), name)
    }

    fun addTrans(src: Int, dest: Int, expr: BooleanExpression) {
        automaton.addTransition(src, dest, InputEvent("REQ"), BooleanExpressionGuard(expr))
    }

    // output-decomposition = MultiArray(shape = [7], values = [3, 3, 2, 3, 3, 1, 2])
    // [I] Module #2 (C = 4, K = 3, P = 5, T = 7, N = 15):
    // Automaton Hash-Code: 819748
    //   ┌─1/ε(0:00, 1:00)
    //   └──1 to 2 on REQ if pp1
    //   ┌─2/CNF(0:00, 1:11)
    //   ├──2 to 3 on REQ if (c1End & vcHome)
    //   └──2 to 4 on REQ if ((c1Home & vac) & vcEnd)
    //   ┌─3/CNF(0:00, 1:11)
    //   ├──3 to 2 on REQ if c1Home
    //   ├──3 to 2 on REQ if (vac & pp1)
    //   └──3 to 3 on REQ if vcEnd
    //   ┌─4/CNF(0:01, 1:11)
    //   └──4 to 2 on REQ if c1Home

    automaton.addState(1, OutputEvent("CNF"), BinaryAlgorithm("00", "00"))
    automaton.addState(2, OutputEvent("CNF"), BinaryAlgorithm("00", "11"))
    automaton.addState(3, OutputEvent("CNF"), BinaryAlgorithm("00", "11"))
    automaton.addState(4, OutputEvent("CNF"), BinaryAlgorithm("01", "11"))

    addTrans(1, 2, getVar("pp1"))
    addTrans(2, 3, getVar("c1End") and getVar("vcHome"))
    addTrans(2, 4, getVar("c1Home") and getVar("vac") and getVar("vcEnd"))
    addTrans(3, 2, getVar("c1Home"))
    addTrans(3, 2, getVar("vac") and getVar("pp1"))
    addTrans(3, 3, getVar("vcEnd"))
    addTrans(4, 2, getVar("c1Home"))

    automaton
}

internal val pnpModule3: Automaton by lazy {
    val inputNames = listOf("c1Home", "c1End", "c2Home", "c2End", "vcHome", "vcEnd", "pp1", "pp2", "pp3", "vac")
    // listOf("c1Extend", "c1Retract", "c2Extend", "c2Retract", "vcExtend", "vacuum_on", "vacuum_off")
    val outputNames = listOf("c1Extend", "c1Retract", "c2Retract", "vcExtend")
    val automaton = Automaton(
        inputEvents = listOf(InputEvent("REQ")),
        outputEvents = listOf(OutputEvent("CNF")),
        inputNames = inputNames,
        outputNames = outputNames,
        initialOutputValues = OutputValues.zeros(outputNames.size)
    )

    fun getVar(name: String): Variable {
        return BooleanExpression.variable(inputNames.indexOf(name), name)
    }

    fun addTrans(src: Int, dest: Int, expr: BooleanExpression) {
        automaton.addTransition(src, dest, InputEvent("REQ"), BooleanExpressionGuard(expr))
    }

    // output-decomposition = MultiArray(shape = [7], values = [3, 3, 2, 3, 3, 1, 2])
    // [I] Module #3 (C = 4, K = 3, P = 4, T = 7, N = 18):
    // Automaton Hash-Code: 194901
    //   ┌─1/ε(0:0000, 1:0000)
    //   └──1 to 2 on REQ if pp1
    //   ┌─2/CNF(0:1000, 1:1110)
    //   ├──2 to 4 on REQ if (vac & vcHome)
    //   └──2 to 3 on REQ if (vcHome & c1End)
    //   ┌─3/CNF(0:0001, 1:1111)
    //   ├──3 to 4 on REQ if (c1Home & ~vac)
    //   ├──3 to 2 on REQ if (c1End & vac)
    //   └──3 to 3 on REQ if vcEnd
    //   ┌─4/CNF(0:0110, 1:0110)
    //   └──4 to 3 on REQ if (c1Home & vac)

    automaton.addState(1, OutputEvent("CNF"), BinaryAlgorithm("0000", "0000"))
    automaton.addState(2, OutputEvent("CNF"), BinaryAlgorithm("1000", "1110"))
    automaton.addState(3, OutputEvent("CNF"), BinaryAlgorithm("0001", "1111"))
    automaton.addState(4, OutputEvent("CNF"), BinaryAlgorithm("0110", "0110"))

    addTrans(1, 2, getVar("pp1"))
    addTrans(2, 4, getVar("vac") and getVar("vcHome"))
    addTrans(2, 3, getVar("vcHome") and getVar("c1End"))
    addTrans(3, 4, getVar("c1Home") and getVar("vac").not())
    addTrans(3, 2, getVar("c1End") and getVar("vac"))
    addTrans(3, 3, getVar("vcEnd"))
    addTrans(4, 3, getVar("c1Home") and getVar("vac"))

    automaton
}

private fun runPnp() {
    Globals.IS_DEBUG = true

    val inputNames = inputNamesPnP
    val outputNames = outputNamesPnP

    val X = inputNames.size
    val Z = outputNames.size

    val modules = multiArrayOf(pnpModule1, pnpModule2, pnpModule3)
    val M = modules.shape.single()
    val inboundVarPinParent = with(Pins(M, X, Z, 1, 1)) {
        (externalOutboundVarPins + externalOutboundVarPins + externalOutboundVarPins + listOf(
            modularOutboundVarPins[1][0],
            modularOutboundVarPins[2][0],
            modularOutboundVarPins[2][1],
            modularOutboundVarPins[3][0],
            modularOutboundVarPins[3][1],
            modularOutboundVarPins[3][2],
            modularOutboundVarPins[3][3],
        )).toMultiArray()
    }
    logger.info("inboundVarPinParent = $inboundVarPinParent")
    check(inboundVarPinParent.shape.single() == X * 3 + Z)

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
        AutomatonBlock("PNP1", pnpModule1),
        AutomatonBlock("PNP2", pnpModule2),
        AutomatonBlock("PNP3", pnpModule3),
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

    runPnp()

    logger.info("All done in %.3f s.".format(timeSince(timeStart).seconds))
}
