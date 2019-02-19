package ru.ifmo.fbsat

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.context
import com.github.ajalt.clikt.output.PlaintextHelpFormatter
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.defaultLazy
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.choice
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.Scenario
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.solver.DefaultSolver
import ru.ifmo.fbsat.solver.IncrementalSolver
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.task.basic.Basic
import ru.ifmo.fbsat.task.basicmin.BasicMin
import ru.ifmo.fbsat.task.extended.Extended
import ru.ifmo.fbsat.task.extendedmin.ExtendedMin
import java.io.File
import kotlin.system.measureTimeMillis

class FbSAT : CliktCommand() {
    private val fileScenarios by option(
        "-i", "--scenarios",
        help = "File with scenarios [required]",
        metavar = "<path>"
    ).file(
        exists = true,
        folderOkay = false,
        readable = true
    ).required()

    private val fileCounterExamples by option(
        "-ce", "--counterexamples",
        help = "File with counter-examples",
        metavar = "<path>"
    ).file(
        exists = true,
        folderOkay = false,
        readable = true
    )

    private val smvDir by option(
        "-smv", "--smvdir",
        help = "Folder with SMV files/scripts for verification",
        metavar = "<path>"
    ).file(
        exists = true,
        fileOkay = false
    )

    private val outDir by option(
        "-o", "--outdir",
        help = "Output directory [default: current directory]",
        metavar = "<path>"
    ).file().defaultLazy { File("") }

    private val method by option(
        "-m", "--method",
        help = "Method to use [required]",
        metavar = "<method>"
    ).choice(
        "basic", "basic-min",
        "extended", "extended-min", "extended-min-ub",
        "extended-ce", "extended-ce-min", "extended-ce-min-ub"
    ).required()

    private val numberOfStates by option(
        "-C",
        help = "Number of automaton states"
    ).int()

    private val maxOutgoingTransitions by option(
        "-K",
        help = "Maximum number of transitions from each state"
    ).int()

    private val maxGuardSize by option(
        "-P",
        help = "Maximum number of nodes in guard's boolean formula's parse tree"
    ).int()

    private val maxTransitions by option(
        "-T",
        help = "Upper bound on total number of transitions in automaton"
    ).int()

    private val maxTotalGuardsSize by option(
        "-N",
        help = "Upper bound on total number of nodes in all guard-trees"
    ).int()

    private val solverCmd by option(
        "--solver",
        help = "SAT-solver [default: $SAT_SOLVER_DEFAULT]",
        metavar = "<cmd>"
    ).default(SAT_SOLVER_DEFAULT)

    private val isIncrementalSolver by option(
        "--incremental",
        help = "Use IncrementalSolver backend [default: true]"
    ).flag(
        "--no-incremental",
        default = true
    )

    private val isForbidLoops by option(
        "--forbid-loops",
        help = "Forbid loops [default: true]"
    ).flag(
        "--no-forbid-loops",
        default = true
    )

    private val failIfSTVerifyFailed by option(
        "--fail-verify-st",
        help = "Halt if verification of scenario tree has failed [default: true]"
    ).flag(
        "--no-fail-verify-st",
        default = true
    )

    private val failIfCEVerifyFailed by option(
        "--fail-verify-ce",
        help = "Halt if verification of counterexamples has failed [default: true]"
    ).flag(
        "--no-fail-verify-ce",
        default = true
    )

    private val fileCE by option(
        "--vis",
        help = "[DEBUG] Visualize given counterexamples via graphviz"
    ).file(
        exists = true,
        folderOkay = false,
        readable = true
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

        val ceTree = fileCounterExamples?.let {
            NegativeScenarioTree.fromFile(
                it,
                tree.inputEvents,
                tree.outputEvents,
                tree.inputNames,
                tree.outputNames
            )
        }
        // =================
        if (ceTree != null) {
            println("ceTree.uniqueInputs: ${ceTree.uniqueInputs}")
            println("Inputs only in ceTree: ${ceTree.uniqueInputs - tree.uniqueInputs}")
        }
        // =================

        // ===
        fileCE?.let { file ->
            println("======================================")
            println("[*] Visualizing <$file>...")
            val negST = NegativeScenarioTree.fromFile(
                file,
                tree.inputEvents,
                tree.outputEvents,
                tree.inputNames,
                tree.outputNames
            )
            File("$file.gv").writeText(negST.toGraphvizString())
            Runtime.getRuntime().exec("dot -Tpdf -O $file.gv")
            // Runtime.getRuntime().exec("dot -Tpng -O ce.gv")

            println("======================================")
            println("[*] Searching for multi-loops...")
            for (v in negST.verticesWithLoops) {
                val loopBacks = negST.loopBacks(v)
                if (loopBacks.size >= 2) {
                    println("[*] Node v = $v has ${loopBacks.size} loop-backs: $loopBacks")
                    for ((i, ns) in negST.counterExamples.withIndex()) {
                        // if (ns.elements.last().nodeId == v) {
                        //     println(" >> NegativeScenario #${i + 1} with loop position ${ns.loopPosition} (id = ${ns.elements[ns.loopPosition!! - 1].nodeId})")
                        // }
                        if (ns.loopPosition != null &&
                            ns.elements[ns.loopPosition - 1].nodeId in loopBacks
                        ) {
                            println(" >> NegativeScenario #${i + 1} with loop position ${ns.loopPosition} (id = ${ns.elements[ns.loopPosition - 1].nodeId})")
                        }
                    }
                }
            }
            println("======================================")
            return
        }
        // ===

        val solverProvider: () -> Solver = if (isIncrementalSolver) {
            { IncrementalSolver(solverCmd) }
        } else {
            { DefaultSolver(solverCmd) }
        }

        val automaton: Automaton? = when (method) {
            "basic" -> {
                val task = Basic(
                    tree,
                    ceTree,
                    numberOfStates!!,
                    maxOutgoingTransitions,
                    solverProvider
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
                    solverProvider
                )
                task.infer()
            }
            "extended" -> {
                val task = Extended(
                    tree,
                    ceTree,
                    numberOfStates!!,
                    maxOutgoingTransitions,
                    maxGuardSize!!,
                    solverProvider,
                    isForbidLoops = isForbidLoops
                )
                task.infer(maxTotalGuardsSize)
            }
            "extended-min" -> {
                val task = ExtendedMin(
                    tree,
                    ceTree,
                    numberOfStates,
                    maxOutgoingTransitions,
                    maxGuardSize!!,
                    maxTotalGuardsSize,
                    solverProvider,
                    isForbidLoops = isForbidLoops
                )
                task.infer()
            }
            // "extended-ce" -> {
            //     val task = ExtendedCE(
            //         tree,
            //         numberOfStates!!,
            //         maxOutgoingTransitions,
            //         maxGuardSize!!,
            //         solverProvider,
            //         smvDir!!
            //     )
            //     task.infer(maxTotalGuardsSize)
            // }
            else -> throw UnsupportedOperationException("Method '$method' is not supported yet.")
        }

        if (automaton == null) {
            println("[-] Automaton not found")
        } else {
            println("[+] Automaton:")
            automaton.pprint()

            println("[+] Automaton has ${automaton.numberOfStates} states, ${automaton.numberOfTransitions} transitions and ${automaton.totalGuardsSize} nodes")

            if (automaton.verify(tree))
                println("[+] Verify: OK")
            else {
                println("==================")
                println("[-] Verify: FAILED")
                println("==================")
                if (failIfSTVerifyFailed)
                    error("ST verification failed")
            }

            ceTree?.let {
                if (automaton.verify(it))
                    println("[+] Verify CE: OK")
                else {
                    println("=====================")
                    println("[-] Verify CE: FAILED")
                    println("=====================")
                    if (failIfCEVerifyFailed)
                        error("CE verification failed")
                }
            }

            outDir.mkdirs()
            automaton.dump(outDir, "automaton")
        }
    }

    companion object {
        private const val SAT_SOLVER_DEFAULT = "incremental-cryptominisat"
    }
}

fun main(args: Array<String>) {
    val runningTime = measureTimeMillis { FbSAT().main(args) }
    println("[+] All done in %.3f seconds".format(runningTime / 1000.0))
}
