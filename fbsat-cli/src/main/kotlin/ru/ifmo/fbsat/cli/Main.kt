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
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.basic.BasicMinTask
import ru.ifmo.fbsat.core.task.basic.BasicTask
import ru.ifmo.fbsat.core.task.basic.ModularBasicMinTask
import ru.ifmo.fbsat.core.task.basic.ModularBasicTask
import ru.ifmo.fbsat.core.task.complete.CompleteCegisTask
import ru.ifmo.fbsat.core.task.complete.CompleteMinCegisTask
import ru.ifmo.fbsat.core.task.complete.CompleteTask
import ru.ifmo.fbsat.core.task.extended.ExtendedMinTask
import ru.ifmo.fbsat.core.task.extended.ExtendedMinUBTask
import ru.ifmo.fbsat.core.task.extended.ExtendedTask
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
        "--smvdir",
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
    ).file().defaultLazy {
        File(LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss")))
    }

    // TODO: enum Method
    private val method by option(
        "-m", "--method",
        help = "Method to use [required]",
        metavar = "<method>"
    ).choice(
        "basic", "basic-min",
        "extended", "extended-min", "extended-min-ub",
        "extended-ce", "extended-min-ce",
        "complete", "complete-min",
        "complete-cegis", "complete-min-cegis",
        "modular-basic", "modular-basic-min"
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

    private val numberOfModules by option(
        "-M",
        help = "Number of modules",
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

        val tree = ScenarioTree.fromFile(fileScenarios, inputNamesPnP, outputNamesPnP)
        println("[*] Scenarios: ${tree.scenarios.size}")
        println("[*] Elements: ${tree.scenarios.sumBy { it.elements.size }}")

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
            { Solver.incremental(solverCmd) }
        } else {
            { Solver.default(solverCmd) }
        }

        val automaton: Automaton? = when (method) {
            "basic" -> {
                val task = BasicTask.create(
                    scenarioTree = tree,
                    numberOfStates = numberOfStates!!,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxTransitions = maxTransitions,
                    solverProvider = solverProvider,
                    outDir = outDir
                )
                task.infer()
            }
            "basic-min" -> {
                val task = BasicMinTask.create(
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
                val task = ExtendedTask.create(
                    scenarioTree = tree,
                    numberOfStates = requireNotNull(numberOfStates),
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = requireNotNull(maxGuardSize),
                    maxTotalGuardsSize = maxTotalGuardsSize,
                    outDir = outDir,
                    solverProvider = solverProvider
                )
                task.infer()
            }
            "extended-min" -> {
                val task = ExtendedMinTask.create(
                    scenarioTree = tree,
                    numberOfStates = numberOfStates,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = requireNotNull(maxGuardSize),
                    initialMaxTotalGuardsSize = maxTotalGuardsSize,
                    outDir = outDir,
                    solverProvider = solverProvider
                )
                task.infer()
            }
            "extended-min-ub" -> {
                val task = ExtendedMinUBTask.create(
                    scenarioTree = tree,
                    initialMaxTotalGuardsSize = maxTotalGuardsSize,
                    maxPlateauWidth = maxPlateauWidth,
                    outDir = outDir,
                    solverProvider = solverProvider
                )
                task.infer()
            }
            "complete" -> {
                val task = CompleteTask.create(
                    scenarioTree = tree,
                    negativeScenarioTree = negTree,
                    numberOfStates = requireNotNull(numberOfStates),
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = requireNotNull(maxGuardSize),
                    maxTotalGuardsSize = maxTotalGuardsSize,
                    outDir = outDir,
                    solverProvider = solverProvider
                )
                task.infer()
            }
            "complete-cegis" -> {
                val task = CompleteCegisTask.create(
                    scenarioTree = tree,
                    negativeScenarioTree = negTree,
                    numberOfStates = requireNotNull(numberOfStates),
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = requireNotNull(maxGuardSize),
                    maxTotalGuardsSize = maxTotalGuardsSize,
                    smvDir = smvDir,
                    outDir = outDir,
                    solverProvider = solverProvider
                )
                task.infer()
            }
            "complete-min-cegis" -> {
                val task = CompleteMinCegisTask.create(
                    scenarioTree = tree,
                    initialNegativeScenarioTree = negTree,
                    numberOfStates = numberOfStates,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = requireNotNull(maxGuardSize),
                    initialMaxTotalGuardsSize = maxTotalGuardsSize,
                    smvDir = smvDir,
                    outDir = outDir,
                    solverProvider = solverProvider
                )
                task.infer()
            }
            "modular-basic" -> {
                val task = ModularBasicTask.create(
                    scenarioTree = tree,
                    numberOfModules = numberOfModules!!,
                    numberOfStates = numberOfStates!!,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxTransitions = maxTransitions,
                    solverProvider = solverProvider,
                    outDir = outDir
                )
                val modularAutomaton = task.infer()

                if (modularAutomaton == null) {
                    log.failure("Modular automaton not found")
                } else {
                    log.info("Inferred modular automaton, consisting of ${modularAutomaton.modules.size} modules:")
                    for ((m, automaton) in modularAutomaton.modules.withIndex()) {
                        log.info("Automaton #${m + 1} has ${automaton.numberOfStates} states and ${automaton.numberOfTransitions} transitions:")
                        automaton.pprint()
                        automaton.dump(outDir, "the_module-$m")
                    }
                    modularAutomaton.dumpFbt(
                        outDir.resolve("modularAutomaton.fbt"),
                        name = "ModularController"
                    )
                    if (modularAutomaton.verify(tree))
                        log.success("Verify: OK")
                    else {
                        log.failure("Verify: FAILED")
                    }
                }

                log.br()
                log.br("The following messages - lies.")
                log.br()
                null
            }
            "modular-basic-min" -> {
                val task = ModularBasicMinTask.create(
                    scenarioTree = tree,
                    numberOfModules = numberOfModules!!,
                    numberOfStates = numberOfStates,
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    initialMaxTransitions = maxTransitions,
                    solverProvider = solverProvider,
                    outDir = outDir
                )
                val modularAutomaton = task.infer()

                if (modularAutomaton == null) {
                    log.failure("Modular automaton not found")
                } else {
                    log.info("Inferred modular automaton, consisting of ${modularAutomaton.modules.size} modules:")
                    for ((m, automaton) in modularAutomaton.modules.withIndex()) {
                        log.info("Automaton #${m + 1} has ${automaton.numberOfStates} states and ${automaton.numberOfTransitions} transitions:")
                        automaton.pprint()
                        automaton.dump(outDir, "the_module-$m")
                    }
                    modularAutomaton.dumpFbt(
                        outDir.resolve("modularAutomaton.fbt"),
                        name = "ModularController"
                    )
                    if (modularAutomaton.verify(tree))
                        log.success("Verify: OK")
                    else {
                        log.failure("Verify: FAILED")
                    }
                }

                log.br()
                log.br("The following messages - lies.")
                log.br()
                null
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
