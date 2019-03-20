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
import ru.ifmo.fbsat.automaton.Algorithm
import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.automaton.StringGuard
import ru.ifmo.fbsat.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.solver.DefaultSolver
import ru.ifmo.fbsat.solver.IncrementalSolver
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.task.basic.BasicTask
import ru.ifmo.fbsat.task.basicmin.BasicMinTask
import ru.ifmo.fbsat.task.extended.ExtendedTask
import ru.ifmo.fbsat.task.extendedce.ExtendedCETask
import ru.ifmo.fbsat.task.extendedmin.ExtendedMinTask
import ru.ifmo.fbsat.task.extendedmince.ExtendedMinCETask
import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import kotlin.system.measureTimeMillis

private const val SAT_SOLVER_DEFAULT = "incremental-cryptominisat"

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

    private val fileCounterexamples by option(
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
    ).defaultLazy { File("data/pnp/smv") }

    private val outDir by option(
        "-o", "--outdir",
        help = "Output directory [default: current directory]",
        metavar = "<path>"
    ).file().defaultLazy { File("") }

    // TODO: enum Method
    private val method by option(
        "-m", "--method",
        help = "Method to use [required]",
        metavar = "<method>"
    ).choice(
        "basic", "basic-min",
        "extended", "extended-min", "extended-min-ub",
        "extended-ce", "extended-min-ce"
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
        help = "Halt if verification of negativeScenarios has failed [default: true]"
    ).flag(
        "--no-fail-verify-ce",
        default = true
    )

    private val fileVis by option(
        "--vis",
        help = "[DEBUG] Visualize given counterexamples via graphviz"
    ).file(
        exists = true,
        folderOkay = false,
        readable = true
    )

    private val isEncodeAutomaton by option(
        "--encode-automaton",
        help = "[DEBUG] Encode Daniil's automaton"
    ).flag(
        default = false
    )

    private val isEncodeTransitionsOrder by option(
        "--encode-transitions-order",
        help = "[DEBUG] Encode transitions lexicographic order"
    ).flag(
        "--no-encode-transitions-order",
        default = false
    )

    private val isOnlyAutomaton2 by option(
        "--only-automaton2"
    ).flag()

    private val fileVerifyCE by option(
        "--verify-ce"
    ).file()

    init {
        context { helpFormatter = PlaintextHelpFormatter(maxWidth = 999) }
    }

    override fun run() {
        val scenarios = PositiveScenario.fromFile(fileScenarios)
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

        val negTree = fileCounterexamples?.let {
            NegativeScenarioTree.fromFile(
                it,
                tree.inputEvents,
                tree.outputEvents,
                tree.inputNames,
                tree.outputNames
            )
        }

        // ===
        fileVis?.let { file ->
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
                    for ((i, ns) in negST.negativeScenarios.withIndex()) {
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

        if (isOnlyAutomaton2) {
            val scenarios4 = PositiveScenario.fromFile(File("data/tests-4"))
            val tree4 = ScenarioTree(scenarios4, tree.inputNames, tree.outputNames)

            val automaton2 = Automaton(tree4)
            fun magic(algo: String): Algorithm {
                return BinaryAlgorithm(
                    algorithm0 = algo.map {
                        when (it) {
                            '0', 'x' -> '0'
                            '1' -> '1'
                            else -> error("Bad char '$it'")
                        }
                    }.joinToString(""),
                    algorithm1 = algo.map {
                        when (it) {
                            '0' -> '0'
                            '1', 'x' -> '1'
                            else -> error("Bad char '$it'")
                        }
                    }.joinToString("")
                )
            }
            automaton2.addState(1, "INITO", magic("0000000"))
            automaton2.addState(2, "CNF", magic("1x1xxxx"))
            automaton2.addState(3, "CNF", magic("xx1x0xx"))
            automaton2.addState(4, "CNF", magic("10x00x0"))
            automaton2.addState(5, "CNF", magic("xxxx1xx"))
            automaton2.addState(6, "CNF", magic("01010xx"))
            automaton2.addState(7, "CNF", magic("xxxxx1x"))
            automaton2.addState(8, "CNF", magic("xxxxx01"))

            automaton2.addTransition(
                1, 2, "REQ",
                StringGuard("pp3", tree.inputNames)
            )
            automaton2.addTransition(
                1, 3, "REQ",
                StringGuard("pp2", tree.inputNames)
            )
            automaton2.addTransition(
                1, 4, "REQ",
                StringGuard("pp1", tree.inputNames)
            )
            automaton2.addTransition(
                2, 5, "REQ",
                StringGuard("c2End", tree.inputNames)
            )
            automaton2.addTransition(
                3, 5, "REQ",
                StringGuard("c2End & !vac", tree.inputNames)
            )
            automaton2.addTransition(
                3, 6, "REQ",
                StringGuard("vcHome & !pp2", tree.inputNames)
            )
            automaton2.addTransition(
                4, 5, "REQ",
                StringGuard("c1End & !vac", tree.inputNames)
            )
            automaton2.addTransition(
                4, 6, "REQ",
                StringGuard("vcHome & vac", tree.inputNames)
            )
            automaton2.addTransition(
                5, 7, "REQ",
                StringGuard("vcEnd & !vac", tree.inputNames)
            )
            automaton2.addTransition(
                5, 8, "REQ",
                StringGuard("vcEnd", tree.inputNames)
            )
            automaton2.addTransition(
                6, 4, "REQ",
                StringGuard("!vcEnd & pp1 & !vac", tree.inputNames)
            )
            automaton2.addTransition(
                6, 5, "REQ",
                StringGuard("c1Home & c2Home & vac", tree.inputNames)
            )
            automaton2.addTransition(
                7, 3, "REQ",
                StringGuard("!pp1", tree.inputNames)
            )
            automaton2.addTransition(
                7, 4, "REQ",
                StringGuard("!pp3", tree.inputNames)
            )
            automaton2.addTransition(
                8, 6, "REQ",
                StringGuard("!vac", tree.inputNames)
            )

            println("[+] Automaton2:")
            automaton2.pprint()
            println("[+] Automaton2 has ${automaton2.numberOfStates} states, ${automaton2.numberOfTransitions} transitions and ${automaton2.totalGuardsSize} nodes")

            if (automaton2.verify(tree4))
                println("[+] Verify automaton2 on tests-4: OK")
            else
                println("[-] Verify automaton2 on tests-4: FAILED")

            if (automaton2.verify(tree))
                println("[+] Verify automaton2 on $fileScenarios: OK")
            else
                println("[-] Verify automaton2 on $fileScenarios: FAILED")

            if (negTree != null)
                if (automaton2.verify(negTree))
                    println("[+] Verify(CE) automaton2 on $fileCounterexamples: OK")
                else
                    println("[-] Verify(CE) automaton2 on $fileCounterexamples: FAILED")
            return
        }

        val automaton: Automaton? = when (method) {
            "basic" -> {
                val task = BasicTask(
                    tree,
                    negTree,
                    numberOfStates!!,
                    maxOutgoingTransitions,
                    solverProvider
                )
                task.infer(maxTransitions)
            }
            "basic-min" -> {
                val task = BasicMinTask(
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
                val task = ExtendedTask(
                    tree,
                    negTree,
                    numberOfStates!!,
                    maxOutgoingTransitions,
                    maxGuardSize!!,
                    solverProvider,
                    isForbidLoops = isForbidLoops,
                    isEncodeAutomaton = isEncodeAutomaton,
                    isEncodeTransitionsOrder = isEncodeTransitionsOrder
                )
                task.infer(maxTotalGuardsSize)
            }
            "extended-min" -> {
                val task = ExtendedMinTask(
                    tree,
                    negTree,
                    numberOfStates,
                    maxOutgoingTransitions,
                    maxGuardSize!!,
                    maxTotalGuardsSize,
                    solverProvider,
                    isForbidLoops = isForbidLoops,
                    isEncodeAutomaton = isEncodeAutomaton,
                    isEncodeTransitionsOrder = isEncodeTransitionsOrder
                )
                task.infer()
            }
            "extended-ce" -> {
                val task = ExtendedCETask(
                    tree,
                    negTree,
                    numberOfStates!!,
                    maxOutgoingTransitions,
                    maxGuardSize!!,
                    solverProvider,
                    smvDir,
                    isEncodeAutomaton = isEncodeAutomaton,
                    isEncodeTransitionsOrder = isEncodeTransitionsOrder
                )
                task.infer(maxTotalGuardsSize)
            }
            "extended-min-ce" -> {
                val task = ExtendedMinCETask(
                    tree,
                    negTree,
                    numberOfStates,
                    maxOutgoingTransitions,
                    maxGuardSize!!,
                    maxTotalGuardsSize,
                    solverProvider,
                    smvDir,
                    isEncodeAutomaton = isEncodeAutomaton,
                    isEncodeTransitionsOrder = isEncodeTransitionsOrder
                )
                task.infer()
            }
            else -> TODO("Method '$method' is not implemented yet.")
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
                println("[-] Verify: FAILED")
                if (failIfSTVerifyFailed)
                    error("ST verification failed")
            }

            if (negTree != null) {
                if (automaton.verify(negTree))
                    println("[+] Verify CE: OK")
                else {
                    println("[-] Verify CE: FAILED")
                    if (failIfCEVerifyFailed)
                        error("CE verification failed")
                }

                // val fileCEMarkedGv = File("ce-marked.gv")
                // fileCEMarkedGv.writeText(negTree.toGraphvizString())
                // Runtime.getRuntime().exec("dot -Tpdf -O $fileCEMarkedGv")
            }

            fileVerifyCE?.let {
                val nst = NegativeScenarioTree.fromFile(
                    it,
                    tree.inputEvents,
                    tree.outputEvents,
                    tree.inputNames,
                    tree.outputNames
                )
                if (automaton.verify(nst))
                    println("[+] Verify CE from '$fileVerifyCE': OK")
                else
                    println("[-] Verify CE from '$fileVerifyCE': FAILED")
            }

            outDir.mkdirs()
            automaton.dump(outDir, "automaton")
        }
    }
}

fun main(args: Array<String>) {
    println("=== ${LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))}")
    val runningTime = measureTimeMillis { FbSAT().main(args) }
    println("[+] All done in %.3f seconds".format(runningTime / 1000.0))
    println("=== ${LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))}")
}
