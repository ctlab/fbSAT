package ru.ifmo.fbsat.cli

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
import ru.ifmo.fbsat.core.automaton.Algorithm
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.StringGuard
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.DefaultSolver
import ru.ifmo.fbsat.core.solver.IncrementalSolver
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.basic.BasicTask
import ru.ifmo.fbsat.core.task.basicmin.BasicMinTask
import ru.ifmo.fbsat.core.task.extended.ExtendedTask
import ru.ifmo.fbsat.core.task.extendedce.ExtendedCETask
import ru.ifmo.fbsat.core.task.extendedmin.ExtendedMinTask
import ru.ifmo.fbsat.core.task.extendedmince.ExtendedMinCETask
import ru.ifmo.fbsat.core.task.extendedminub.ExtendedMinUBTask
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.inputNamesPnP
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.outputNamesPnP
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
        help = "Directory with SMV files/scripts for verification",
        metavar = "<path>"
    ).file(
        exists = true,
        fileOkay = false
    ).defaultLazy { File("data/pnp/smv") }

    private val outDir by option(
        "-o", "--outdir",
        help = "Output directory [default: current directory]",
        metavar = "<path>"
    ).file().defaultLazy { File(".") }

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
        help = "Number of automaton states",
        metavar = "<int>"
    ).int()

    private val maxOutgoingTransitions by option(
        "-K",
        help = "Maximum number of transitions from each state",
        metavar = "<int>"
    ).int()

    private val maxGuardSize by option(
        "-P",
        help = "Maximum guard size (number of parse tree nodes)",
        metavar = "<int>"
    ).int()

    private val maxTransitions by option(
        "-T",
        help = "Upper bound for the total number of transitions",
        metavar = "<int>"
    ).int()

    private val maxTotalGuardsSize by option(
        "-N",
        help = "Upper bound for the total size of guards",
        metavar = "<int>"
    ).int()

    private val maxPlateauWidth by option(
        "-w",
        help = "Maximum plateau width",
        metavar = "<int>"
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

    private val isForbidOr by option(
        "--forbid-or"
    ).flag(
        "--no-forbid-or",
        default = false
    )

    private val isBfsAutomaton by option(
        "--bfs-automaton"
    ).flag(
        "--no-bfs-automaton",
        default = true
    )

    private val isBfsGuard by option(
        "--bfs-guard"
    ).flag(
        "--no-bfs-guard",
        default = false
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
        help = "Halt if verification of negative scenarios has failed [default: true]"
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
        help = "[DEBUG] Encode Daniil's automaton [default: false]"
    ).flag(
        default = false
    )

    private val isEncodeTransitionsOrder by option(
        "--encode-transitions-order",
        help = "[DEBUG] Encode transitions lexicographic order [default: false]"
    ).flag(
        "--no-encode-transitions-order",
        default = false
    )

    private val isEncodeTerminalsOrder by option(
        "--encode-terminals-order",
        help = "[DEBUG] Encode terminal numbers lexicographic order [default: true]"
    ).flag(
        "--no-encode-terminals-order",
        default = true
    )

    private val isEncodeReverseImplication by option(
        "--encode-reverse-implication"
    ).flag(
        "--no-encode-reverse-implication",
        default = false
    )

    private val isOnlyAutomaton2 by option(
        "--only-automaton2"
    ).flag()

    private val fileVerifyCE by option(
        "--verify-ce"
    ).file()

    private val isDebug by option(
        "--debug",
        help = "Debug mode [default: false]"
    ).flag(
        default = false
    )

    init {
        context { helpFormatter = PlaintextHelpFormatter(maxWidth = 999) }
    }

    override fun run() {
        Globals.IS_FORBID_OR = isForbidOr
        Globals.IS_BFS_AUTOMATON = isBfsAutomaton
        Globals.IS_BFS_GUARD = isBfsGuard
        Globals.IS_ENCODE_TRANSITIONS_ORDER = isEncodeTransitionsOrder
        Globals.IS_ENCODE_TERMINALS_ORDER = isEncodeTerminalsOrder
        Globals.IS_DEBUG = isDebug

        // outDir.deleteRecursively()
        // outDir.walkBottomUp().forEach { if (it != outDir) it.delete() }
        outDir.mkdirs()

        val scenarios = PositiveScenario.fromFile(fileScenarios)
        println("[*] Scenarios: ${scenarios.size}")
        println("[*] Elements: ${scenarios.sumBy { it.elements.size }}")

        val tree = ScenarioTree(
            scenarios,
            inputNames = inputNamesPnP,
            outputNames = outputNamesPnP
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
            Runtime.getRuntime().exec("dot -Tpdf -O $file.gv").waitFor()
            // Runtime.getRuntime().exec("dot -Tpng -O ce.gv").waitFor()

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
                            ns.elements[ns.loopPosition!! - 1].nodeId in loopBacks
                        ) {
                            println(" >> NegativeScenario #${i + 1} with loop position ${ns.loopPosition} (id = ${ns.elements[ns.loopPosition!! - 1].nodeId})")
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
            val tree4 =
                ScenarioTree(scenarios4, tree.inputNames, tree.outputNames)

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
                    scenarioTree = tree,
                    numberOfStates = numberOfStates!!,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxTransitions = maxTransitions,
                    solver = solverProvider(),
                    outDir = outDir
                )
                task.infer()
            }
            "basic-min" -> {
                val task = BasicMinTask(
                    scenarioTree = tree,
                    numberOfStates = numberOfStates,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    initialMaxTransitions = maxTransitions,
                    solverProvider = solverProvider,
                    outDir = outDir
                )
                task.infer()
            }
            "extended" -> {
                val task = ExtendedTask(
                    scenarioTree = tree,
                    negativeScenarioTree = negTree,
                    numberOfStates = numberOfStates!!,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = maxGuardSize!!,
                    solver = solverProvider(),
                    outDir = outDir,
                    isForbidLoops = isForbidLoops,
                    isEncodeAutomaton = isEncodeAutomaton,
                    isEncodeTransitionsOrder = isEncodeTransitionsOrder,
                    isEncodeReverseImplication = isEncodeReverseImplication
                )
                task.infer(maxTotalGuardsSize)
            }
            "extended-min" -> {
                val task = ExtendedMinTask(
                    scenarioTree = tree,
                    negativeScenarioTree = negTree,
                    numberOfStates = numberOfStates,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = maxGuardSize!!,
                    initialMaxTotalGuardsSize = maxTotalGuardsSize,
                    solverProvider = solverProvider,
                    outDir = outDir,
                    isForbidLoops = isForbidLoops,
                    isEncodeAutomaton = isEncodeAutomaton,
                    isEncodeTransitionsOrder = isEncodeTransitionsOrder,
                    isEncodeReverseImplication = isEncodeReverseImplication
                )
                task.infer()
            }
            "extended-min-ub" -> {
                val task = ExtendedMinUBTask(
                    scenarioTree = tree,
                    negativeScenarioTree = negTree,
                    initialMaxTotalGuardsSize = maxTotalGuardsSize,
                    maxPlateauWidth = maxPlateauWidth,
                    outDir = outDir,
                    solverProvider = solverProvider,
                    isForbidLoops = isForbidLoops,
                    isEncodeAutomaton = isEncodeAutomaton,
                    isEncodeTransitionsOrder = isEncodeTransitionsOrder,
                    isEncodeReverseImplication = isEncodeReverseImplication
                )
                task.infer()
            }
            "extended-ce" -> {
                val task = ExtendedCETask(
                    scenarioTree = tree,
                    initialNegativeScenarioTree = negTree,
                    numberOfStates = numberOfStates!!,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = maxGuardSize!!,
                    outDir = outDir,
                    smvDir = smvDir,
                    solverProvider = solverProvider,
                    isEncodeAutomaton = isEncodeAutomaton,
                    isEncodeTransitionsOrder = isEncodeTransitionsOrder
                )
                task.infer(maxTotalGuardsSize)
            }
            "extended-min-ce" -> {
                val task = ExtendedMinCETask(
                    scenarioTree = tree,
                    initialNegativeScenarioTree = negTree,
                    numberOfStates = numberOfStates,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = maxGuardSize!!,
                    initialMaxTotalGuardsSize = maxTotalGuardsSize,
                    outDir = outDir,
                    smvDir = smvDir,
                    solverProvider = solverProvider,
                    isEncodeAutomaton = isEncodeAutomaton,
                    isEncodeTransitionsOrder = isEncodeTransitionsOrder
                )
                task.infer()
            }
            else -> TODO("method '$method'")
        }

        if (automaton == null) {
            log.failure("Automaton not found")
        } else {
            log.info("Inferred automaton:")
            automaton.pprint()
            log.info("Inferred automaton has ${automaton.numberOfStates} states, ${automaton.numberOfTransitions} transitions and ${automaton.totalGuardsSize} nodes")

            if (automaton.verify(tree))
                log.success("Verify: OK")
            else {
                log.failure("Verify: FAILED")
                if (failIfSTVerifyFailed) error("ST verification failed")
            }

            if (negTree != null) {
                if (automaton.verify(negTree))
                    log.success("Verify CE: OK")
                else {
                    log.failure("Verify CE: FAILED")
                    if (failIfCEVerifyFailed) error("CE verification failed")
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
                    log.success("Verify CE from '$fileVerifyCE': OK")
                else
                    log.failure("Verify CE from '$fileVerifyCE': FAILED")
            }

            automaton.dump(outDir, "automaton")
        }
    }
}

fun main(args: Array<String>) {
    log.br(LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")))
    val runningTime = measureTimeMillis { FbSAT().main(args) }
    log.br(LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")))
    log.success("All done in %.3f seconds".format(runningTime / 1000.0))
}
