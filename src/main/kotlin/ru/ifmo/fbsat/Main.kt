package ru.ifmo.fbsat

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.int
import ru.ifmo.fbsat.scenario.CounterExample
import ru.ifmo.fbsat.scenario.CounterExampleTree
import ru.ifmo.fbsat.scenario.Scenario
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.IncrementalSolver
import ru.ifmo.fbsat.task.extendedmin.ExtendedMin
import kotlin.system.measureTimeMillis

@Suppress("MemberVisibilityCanBePrivate")
class Application : CliktCommand() {
    val filenameScenarios by option(
        "-i",
        "--scenarios",
        help = "File with scenarios"
    ).required()
    val numberOfStates by option(
        "-C",
        help = "Number of automaton states"
    ).int().required()
    val maxOutgoingTransitions by option(
        "-K",
        help = "Maximum number of transitions from each state"
    ).int()
    val maxGuardSize by option(
        "-P",
        help = "Maximum number of nodes in guard's boolean formula's parse tree"
    ).int().required()
    val maxTotalGuardsSize by option(
        "-N",
        help = "Upper bound on total number of nodes in all guard-trees"
    ).int()

    override fun run() {
        val scenarios = Scenario.fromFile(filenameScenarios)
        println("[*] Scenarios: ${scenarios.size}")
        println("[*] Elements: ${scenarios.sumBy { it.elements.size }}")

        val tree = ScenarioTree(
            scenarios,
            inputNames = listOf("c1Home", "c1End", "c2Home", "c2End", "vcHome", "vcEnd", "pp1", "pp2", "pp3", "vac"),
            outputNames = listOf(
                "c1Extend",
                "c1Retract",
                "c2Extend",
                "c2Retract",
                "vcExtend",
                "vacuum_on",
                "vacuum_off"
            )
        )
        val counterExamples = CounterExample.fromFile(
            "ce",
            tree.inputEvents,
            tree.outputEvents,
            tree.inputNames,
            tree.outputNames
        )
        val ceTree = CounterExampleTree(
            counterExamples,
            inputNames = tree.inputNames,
            outputNames = tree.outputNames,
            isTrie = false
        )
        // =================
        val _a = tree.uniqueInputs
        val _b = ceTree.uniqueInputs
        val _c = _a - _b
        val _d = _b - _a
        println("ceTree.uniqueInputs: $_b")
        println("Inputs only in ceTree: $_d")
        // =================
//        val task = Extended(
//            tree,
//            ceTree,
//            numberOfStates,
//            maxOutgoingTransitions,
//            maxGuardSize,
//            { IncrementalSolver("incremental-cryptominisat") }
//        )
//        val automaton = task.infer(maxTotalGuardsSize)
        val task = ExtendedMin(
            tree,
            ceTree,
            numberOfStates,
            maxOutgoingTransitions,
            maxGuardSize,
            maxTotalGuardsSize,
            { IncrementalSolver("incremental-cryptominisat") }
        )
        val automaton = task.infer()

        if (automaton == null) {
            println("[-] Automaton not found")
        } else {
            println("[+] Automaton:")
            automaton.pprint()
            println("[+] Automaton has ${automaton.numberOfStates} states, ${automaton.numberOfTransitions} transitions and ${automaton.totalGuardsSize} nodes")
            automaton.verify(tree)
            task.counterExampleTree?.let { automaton.verify(it) }

            automaton.dump("automaton")
        }
    }
}

fun main(args: Array<String>) {
    val runningTime = measureTimeMillis { Application().main(args) }
    println("[+] All done in %.3f seconds".format(runningTime / 1000.0))
}
