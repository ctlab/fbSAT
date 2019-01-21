package ru.ifmo.fbsat

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.context
import com.github.ajalt.clikt.output.PlaintextHelpFormatter
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.choice
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.CounterExample
import ru.ifmo.fbsat.scenario.CounterExampleTree
import ru.ifmo.fbsat.scenario.Scenario
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.DefaultSolver
import ru.ifmo.fbsat.solver.IncrementalSolver
import ru.ifmo.fbsat.task.basic.Basic
import ru.ifmo.fbsat.task.basicmin.BasicMin
import ru.ifmo.fbsat.task.extended.Extended
import ru.ifmo.fbsat.task.extendedmin.ExtendedMin
import kotlin.system.measureTimeMillis

@Suppress("MemberVisibilityCanBePrivate")
class FbSAT : CliktCommand() {
    val fileScenarios by option(
        "-i", "--scenarios",
        help = "File with scenarios",
        metavar = "<path>"
    ).file(
        exists = true,
        folderOkay = false,
        readable = true
    ).required()

    val fileCounterExamples by option(
        "-ce", "--counterexamples",
        help = "File with counter-examples",
        metavar = "<path>"
    ).file(
        exists = true,
        folderOkay = false,
        readable = true
    )

    val method by option(
        "-m", "--method",
        help = "Method to use"
    ).choice(
        "basic", "basic-min",
        "extended", "extended-min", "extended-min-ub"
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

    val maxTransitions by option(
        "-T",
        help = "Upper bound on total number of transitions in automaton"
    ).int()

    val maxTotalGuardsSize by option(
        "-N",
        help = "Upper bound on total number of nodes in all guard-trees"
    ).int()

    val solverCmd by option(
        "--solver",
        help = "SAT-solver",
        metavar = "<cmd>"
    ).default(
        // "cryptominisat5"
        "incremental-cryptominisat"
    )

    val isIncrementalSolver by option(
        "--incremental",
        help = "Use IncrementalSolver backend"
    ).flag(
        "--no-incremental",
        // default = false
        default = true
    )

    init {
        context { helpFormatter = PlaintextHelpFormatter(maxWidth = 999) }
    }

    override fun run() {
        val scenarios = Scenario.fromFile(fileScenarios)
        println("[*] Scenarios: ${scenarios.size}")
        println("[*] Elements: ${scenarios.sumBy { it.elements.size }}")

        val tree = ScenarioTree(
            scenarios,
            inputNames = listOf(
                "c1Home",
                "c1End",
                "c2Home",
                "c2End",
                "vcHome",
                "vcEnd",
                "pp1",
                "pp2",
                "pp3",
                "vac"
            ),
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

        val ceTree = fileCounterExamples?.let { fileCounterExamples ->
            val counterExamples = CounterExample.fromFile(
                fileCounterExamples,
                tree.inputEvents,
                tree.outputEvents,
                tree.inputNames,
                tree.outputNames
            )
            CounterExampleTree(
                counterExamples,
                inputNames = tree.inputNames,
                outputNames = tree.outputNames,
                isTrie = false
            )
        }
        // =================
        if (ceTree != null) {
            println("ceTree.uniqueInputs: ${ceTree.uniqueInputs}")
            println("Inputs only in ceTree: ${ceTree.uniqueInputs - tree.uniqueInputs}")
        }
        // =================

        val solverProducer = if (isIncrementalSolver) {
            { IncrementalSolver(solverCmd) }
        } else {
            { DefaultSolver(solverCmd) }
        }

        val automaton: Automaton? = when (method) {
            "basic" -> {
                val task = Basic(
                    tree,
                    ceTree,
                    numberOfStates,
                    maxOutgoingTransitions,
                    solverProducer
                )
                task.infer(maxTransitions)
            }
            "basic-min" -> {
                val task = BasicMin(
                    tree,
                    // ceTree,
                    numberOfStates,
                    maxOutgoingTransitions,
                    maxTransitions,
                    solverProducer
                )
                task.infer()
            }
            "extended" -> {
                val task = Extended(
                    tree,
                    ceTree,
                    numberOfStates,
                    maxOutgoingTransitions,
                    maxGuardSize,
                    solverProducer
                )
                task.infer(maxTotalGuardsSize)
            }
            "extended-min" -> {
                val task = ExtendedMin(
                    tree,
                    ceTree,
                    numberOfStates,
                    maxOutgoingTransitions,
                    maxGuardSize,
                    maxTotalGuardsSize,
                    solverProducer
                )
                task.infer()
            }
            else -> throw UnsupportedOperationException("Unsupported method '$method'")
        }

        if (automaton == null) {
            println("[-] Automaton not found")
        } else {
            println("[+] Automaton:")
            automaton.pprint()
            println("[+] Automaton has ${automaton.numberOfStates} states, ${automaton.numberOfTransitions} transitions and ${automaton.totalGuardsSize} nodes")
            automaton.verify(tree)
            ceTree?.let { automaton.verify(it) }

            automaton.dump("automaton")
        }
    }
}

fun main(args: Array<String>) {
    val runningTime = measureTimeMillis { FbSAT().main(args) }
    println("[+] All done in %.3f seconds".format(runningTime / 1000.0))
}
