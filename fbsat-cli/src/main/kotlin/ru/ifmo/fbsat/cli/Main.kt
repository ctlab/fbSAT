package ru.ifmo.fbsat.cli

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.context
import com.github.ajalt.clikt.output.CliktHelpFormatter
import com.github.ajalt.clikt.parameters.options.*
import com.github.ajalt.clikt.parameters.types.choice
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import com.soywiz.klock.DateTime
import com.soywiz.klock.measureTime
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.OutputValues
import ru.ifmo.fbsat.core.automaton.minimizeTruthTableGuards
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.arbitraryModularBasic
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.arbitraryModularBasicMin
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.consecutiveModularBasic
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.consecutiveModularBasicMin
import ru.ifmo.fbsat.core.task.modular.basic.parallel.parallelModularBasic
import ru.ifmo.fbsat.core.task.modular.basic.parallel.parallelModularBasicMin
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.consecutiveModularExtended
import ru.ifmo.fbsat.core.task.single.basic.basic
import ru.ifmo.fbsat.core.task.single.basic.basicMin
import ru.ifmo.fbsat.core.task.single.basic.basicMinC
import ru.ifmo.fbsat.core.task.single.complete.cegis
import ru.ifmo.fbsat.core.task.single.complete.cegisMin
import ru.ifmo.fbsat.core.task.single.complete.complete
import ru.ifmo.fbsat.core.task.single.extended.extended
import ru.ifmo.fbsat.core.task.single.extended.extendedMin
import ru.ifmo.fbsat.core.task.single.extended.extendedMinUB
import ru.ifmo.fbsat.core.task.single.extforest.extForest
import ru.ifmo.fbsat.core.task.single.extforest.extForestMin
import ru.ifmo.fbsat.core.utils.EpsilonOutputEvents
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.SolverBackend
import ru.ifmo.fbsat.core.utils.StartStateAlgorithms
import ru.ifmo.fbsat.core.utils.inputNamesPnP
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.outputNamesPnP
import ru.ifmo.fbsat.core.utils.withIndex
import java.io.File

enum class Method(val s: String) {
    Basic("basic"),
    BasicMin("basic-min"),
    Extended("extended"),
    ExtendedMin("extended-min"),
    ExtendedMinUB("extended-min-ub"),
    ExtForest("extforest"),
    ExtForestMin("extforest-min"),
    Complete("complete"),
    CompleteMin("complete-min"),
    Cegis("cegis"),
    CegisMin("cegis-min"),
    ModularBasic("modular-basic"),
    ModularBasicMin("modular-basic-min"),
    ConsecutiveModularBasic("modular-consecutive-basic"),
    ConsecutiveModularBasicMin("modular-consecutive-basic-min"),
    ConsecutiveModularExtended("modular-consecutive-extended"),
    ArbitraryModularBasic("modular-arbitrary-basic"),
    ArbitraryModularBasicMin("modular-arbitrary-basic-min"),
}

@Suppress("MemberVisibilityCanBePrivate")
class FbSAT : CliktCommand() {
    val fileScenarios: File by option(
        "-i", "--scenarios",
        help = "File with scenarios",
        metavar = "<path>"
    ).file(
        exists = true,
        folderOkay = false,
        readable = true
    ).required()

    val fileCounterexamples: File? by option(
        "-ce", "--counterexamples",
        help = "File with counter-examples",
        metavar = "<path>"
    ).file(
        exists = true,
        folderOkay = false,
        readable = true
    )

    val smvDir: File by option(
        "--smvdir",
        help = "Directory with SMV files",
        metavar = "<path>"
    ).file(
        exists = true,
        fileOkay = false
    ).default(
        File("data/pnp/smv")
    )

    val outDir: File by option(
        "-o", "--outdir",
        help = "Output directory",
        metavar = "<path>"
    ).file().defaultLazy {
        val now = DateTime.nowLocal().format("yyyy-MM-dd_HH-mm-ss")
        File("out/$now")
    }

    val fileInputNames: File? by option(
        "--input-names",
        help = "File with input names [defaults to PnP names]"
    ).file(
        exists = true,
        folderOkay = false,
        readable = true
    )

    val fileOutputNames: File? by option(
        "--output-names",
        help = "File with output names [defaults to PnP names]"
    ).file(
        exists = true,
        folderOkay = false,
        readable = true
    )

    val method: Method by option(
        "-m", "--method",
        help = "Method to use",
        metavar = "<method>"
    ).choice(
        Method.values().associateBy { it.s }
    ).required()

    val numberOfStates: Int? by option(
        "-C",
        help = "Number of automaton states",
        metavar = "<int>"
    ).int()

    val maxOutgoingTransitions: Int? by option(
        "-K",
        help = "Maximum number of transitions from each state",
        metavar = "<int>"
    ).int()

    val maxGuardSize: Int? by option(
        "-P",
        help = "Maximum guard size (number of parse tree nodes)",
        metavar = "<int>"
    ).int()

    val maxTransitions: Int? by option(
        "-T",
        help = "Upper bound for the total number of transitions",
        metavar = "<int>"
    ).int()

    val maxTotalGuardsSize: Int? by option(
        "-N",
        help = "Upper bound for the total size of guards",
        metavar = "<int>"
    ).int()

    val maxPlateauWidth: Int? by option(
        "-w",
        help = "Maximum plateau width",
        metavar = "<int>"
    ).int()

    val numberOfModules: Int? by option(
        "-M",
        help = "Number of modules",
        metavar = "<int>"
    ).int()

    val solverCmd: String by option(
        "--solver",
        help = "SAT-solver",
        metavar = "<cmd>"
    ).default(
        "incremental-cryptominisat"
    )

    val solverBackend: SolverBackend by option(
        help = "Solver backend (default: IncrementalSolver)"
    ).switch(
        "--incremental" to SolverBackend.INCREMENTAL,
        "--no-incremental" to SolverBackend.DEFAULT,
        "--file-solver" to SolverBackend.FILE,
        "--minisat" to SolverBackend.MINISAT,
        "--cadical" to SolverBackend.CADICAL
    ).default(
        SolverBackend.INCREMENTAL
    )

    val isForbidOr: Boolean by option(
        "--forbid-or"
    ).flag(
        "--allow-or",
        default = Globals.IS_FORBID_OR
    )

    val isForbidTransitionsToFirstState: Boolean by option(
        "--forbid-transitions-to-first-state"
    ).flag(
        "--allow-transitions-to-first-state",
        default = Globals.IS_FORBID_TRANSITIONS_TO_FIRST_STATE
    )

    val isBfsAutomaton: Boolean by option(
        "--bfs-automaton"
    ).flag(
        "--no-bfs-automaton",
        default = Globals.IS_BFS_AUTOMATON
    )

    val isBfsGuard: Boolean by option(
        "--bfs-guard"
    ).flag(
        "--no-bfs-guard",
        default = Globals.IS_BFS_GUARD
    )

    val isOnlyC: Boolean by option(
        "--only-C",
        help = "[basic-min] Minimize only C, without T"
    ).flag()

    val failIfSTVerifyFailed: Boolean by option(
        "--fail-verify-st",
        help = "Halt if verification of scenario tree has failed"
    ).flag(
        "--no-fail-verify-st",
        default = true
    )

    val failIfCEVerifyFailed: Boolean by option(
        "--fail-verify-ce",
        help = "Halt if verification of negative scenarios has failed"
    ).flag(
        "--no-fail-verify-ce",
        default = true
    )

    val fileVis: File? by option(
        "--vis",
        help = "[DEBUG] Visualize given counterexamples via graphviz"
    ).file(
        exists = true,
        folderOkay = false,
        readable = true
    )

    val epsilonOutputEvents: EpsilonOutputEvents by option(
        "--epsilon-output-events",
        help = "Epsilon output events"
    ).choice(
        "start" to EpsilonOutputEvents.START,
        "onlystart" to EpsilonOutputEvents.ONLYSTART,
        "none" to EpsilonOutputEvents.NONE
    ).default(
        Globals.EPSILON_OUTPUT_EVENTS
    )

    val startStateAlgorithms: StartStateAlgorithms by option(
        "--start-state-algorithms",
        help = "Start state algorithms"
    ).choice(
        "nothing" to StartStateAlgorithms.NOTHING,
        "zero" to StartStateAlgorithms.ZERO,
        "zeronothing" to StartStateAlgorithms.ZERONOTHING,
        "any" to StartStateAlgorithms.ANY,
        "init" to StartStateAlgorithms.INIT,
        "initnothing" to StartStateAlgorithms.INITNOTHING
    ).default(
        Globals.START_STATE_ALGORITHMS
    )

    val isEncodeReverseImplication: Boolean by option(
        "--encode-reverse-implication",
        help = "Encode reverse implication"
    ).flag(
        "--no-encode-reverse-implication",
        default = true
    )

    val isEncodeTransitionsOrder: Boolean by option(
        "--encode-transitions-order",
        help = "[DEBUG] Encode transitions lexicographic order"
    ).flag(
        "--no-encode-transitions-order",
        default = Globals.IS_ENCODE_TRANSITIONS_ORDER
    )

    val isEncodeTerminalsOrder: Boolean by option(
        "--encode-terminals-order",
        help = "[DEBUG] Encode terminal numbers lexicographic order"
    ).flag(
        "--no-encode-terminals-order",
        default = Globals.IS_ENCODE_TERMINALS_ORDER
    )

    val isEncodeTerminalsMiniOrder: Boolean by option(
        "--encode-terminals-mini-order",
        help = "[DEBUG] Encode AND/OR children-terminals order"
    ).flag(
        "--no-encode-terminals-mini-order",
        default = Globals.IS_ENCODE_TERMINALS_MINI_ORDER
    )

    val isEncodeHardToExplain: Boolean by option(
        "--encode-hard-to-explain",
        help = "[DEBUG] Encode some hard to explain thing"
    ).flag(
        "--no-encode-hard-to-explain",
        default = Globals.IS_ENCODE_HARD_TO_EXPLAIN
    )

    val isEncodeTotalizer: Boolean by option(
        "--encode-totalizer",
        help = "Encode totalizer when upper bound is null"
    ).flag(
        "--no-encode-totalizer",
        default = Globals.IS_ENCODE_TOTALIZER
    )

    val isEncodeDisjunctiveTransitions: Boolean by option(
        "--encode-disjunctive-transitions",
        help = "Encode disjunctive transitions (adhocly forbid priority function)"
    ).flag(
        "--no-encode-disjunctive-transitions",
        default = Globals.IS_ENCODE_DISJUNCTIVE_TRANSITIONS
    )

    val isReuseK: Boolean by option(
        "--reuse-k",
        help = "Reuse K found by ExtendedMinTask during CEGIS"
    ).flag(
        "--no-reuse-k",
        default = Globals.IS_REUSE_K
    )

    val fileVerifyCE: File? by option(
        "--verify-ce"
    ).file()

    val isDebug: Boolean by option(
        "--debug",
        help = "Debug mode"
    ).flag(
        default = Globals.IS_DEBUG
    )

    val initialOutputValues: String? by option(
        "--initial-output-values"
    ).validate { it.matches(Regex("[01]+")) }

    init {
        context {
            helpFormatter = CliktHelpFormatter(
                maxWidth = 999,
                requiredOptionMarker = "*",
                showDefaultValues = true,
                showRequiredTag = true
            )
        }
    }

    override fun run() {
        Globals.EPSILON_OUTPUT_EVENTS = epsilonOutputEvents
        Globals.START_STATE_ALGORITHMS = startStateAlgorithms
        Globals.IS_FORBID_OR = isForbidOr
        Globals.IS_FORBID_TRANSITIONS_TO_FIRST_STATE = isForbidTransitionsToFirstState
        Globals.IS_BFS_AUTOMATON = isBfsAutomaton
        Globals.IS_BFS_GUARD = isBfsGuard
        Globals.IS_ENCODE_TRANSITIONS_ORDER = isEncodeTransitionsOrder
        Globals.IS_ENCODE_TERMINALS_ORDER = isEncodeTerminalsOrder
        Globals.IS_ENCODE_TERMINALS_MINI_ORDER = isEncodeTerminalsMiniOrder
        Globals.IS_ENCODE_HARD_TO_EXPLAIN = isEncodeHardToExplain
        Globals.IS_ENCODE_TOTALIZER = isEncodeTotalizer
        Globals.IS_ENCODE_DISJUNCTIVE_TRANSITIONS = isEncodeDisjunctiveTransitions
        Globals.IS_REUSE_K = isReuseK
        Globals.IS_DEBUG = isDebug

        // outDir.deleteRecursively()
        // outDir.walkBottomUp().forEach { if (it != outDir) it.delete() }
        outDir.mkdirs()
        check(outDir.exists()) { "Output directory does not exist" }

        val inputNames = fileInputNames?.readLines() ?: inputNamesPnP
        val outputNames = fileOutputNames?.readLines() ?: outputNamesPnP
        log.info("Input names: $inputNames")
        log.info("Output names: $outputNames")

        Globals.INITIAL_OUTPUT_VALUES = if (initialOutputValues != null)
            OutputValues(initialOutputValues!!.map { c -> c == '1' }.toList())
        else
            OutputValues.zeros(outputNames.size)

        val tree = ScenarioTree.fromFile(fileScenarios, inputNames, outputNames)
        log.info("Scenarios: ${tree.scenarios.size}")
        log.info("Elements: ${tree.scenarios.sumBy { it.elements.size }}")
        log.info("Scenario tree size: ${tree.size}")

        Globals.INITIAL_OUTPUT_VALUES?.let {
            check(tree.outputNames.size == it.size) {
                "Initial values size must be equal to the number of output variables"
            }
        }

        val negTree = fileCounterexamples?.let {
            NegativeScenarioTree.fromFile(
                it,
                tree.inputEvents,
                tree.outputEvents,
                tree.inputNames,
                tree.outputNames
            ).also { negTree ->
                log.info("Negative scenarios: ${negTree.negativeScenarios.size}")
                log.info("Negative elements: ${negTree.negativeScenarios.sumBy { it.elements.size }}")
            }
        }

        // ===
        fileVis?.let { file ->
            println("======================================")
            log.info("Visualizing <$file>...")
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
            log.info("Searching for multi-loops...")
            for (v in negST.verticesWithLoops) {
                val loopBacks = negST.loopBacks(v)
                if (loopBacks.size >= 2) {
                    log.info("Node v = $v has ${loopBacks.size} loop-backs: $loopBacks")
                    for ((i, ns) in negST.negativeScenarios.withIndex()) {
                        // if (ns.elements.last().nodeId == v) {
                        //     log.just(" >> NegativeScenario #${i + 1} with loop position ${ns.loopPosition} (id = ${ns.elements[ns.loopPosition!! - 1].nodeId})")
                        // }
                        if (ns.loopPosition != null &&
                            ns.elements[ns.loopPosition!! - 1].nodeId in loopBacks
                        ) {
                            log.just(" >> NegativeScenario #${i + 1} with loop position ${ns.loopPosition} (id = ${ns.elements[ns.loopPosition!! - 1].nodeId})")
                        }
                    }
                }
            }
            println("======================================")
            return
        }
        // ===

        val solverProvider: () -> Solver = when (solverBackend) {
            SolverBackend.DEFAULT -> {
                { Solver.default(solverCmd) }
            }
            SolverBackend.INCREMENTAL -> {
                { Solver.incremental() }
            }
            SolverBackend.FILE -> {
                { Solver.filesolver(solverCmd, File("cnf")) }
            }
            SolverBackend.MINISAT -> {
                { Solver.minisat() }
            }
            SolverBackend.CADICAL -> {
                { Solver.cadical() }
            }
        }

        // TODO: warn about ignored specified parameters

        val inferrer = Inferrer(solverProvider(), outDir)

        val automaton: Automaton? = when (method) {
            Method.Basic -> {
                inferrer.basic(
                    scenarioTree = tree,
                    numberOfStates = requireNotNull(numberOfStates),
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxTransitions = maxTransitions,
                    isEncodeReverseImplication = isEncodeReverseImplication
                )
            }
            Method.BasicMin -> {
                if (isOnlyC)
                    inferrer.basicMinC(scenarioTree = tree)
                else
                    inferrer.basicMin(scenarioTree = tree)
            }
            Method.Extended -> {
                inferrer.extended(
                    scenarioTree = tree,
                    numberOfStates = requireNotNull(numberOfStates),
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = requireNotNull(maxGuardSize),
                    maxTransitions = maxTransitions,
                    maxTotalGuardsSize = maxTotalGuardsSize,
                    isEncodeReverseImplication = isEncodeReverseImplication
                )
            }
            Method.ExtendedMin -> {
                inferrer.extendedMin(
                    scenarioTree = tree,
                    numberOfStates = numberOfStates,
                    maxGuardSize = requireNotNull(maxGuardSize)
                )
            }
            Method.ExtendedMinUB -> {
                inferrer.extendedMinUB(
                    scenarioTree = tree,
                    numberOfStates = numberOfStates,
                    maxPlateauWidth = maxPlateauWidth
                )
            }
            Method.ExtForest -> {
                inferrer.extForest(
                    scenarioTree = tree,
                    numberOfStates = requireNotNull(numberOfStates),
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    totalNodes = requireNotNull(maxGuardSize),
                    maxTransitions = maxTransitions,
                    maxTotalGuardsSize = maxTotalGuardsSize,
                    isEncodeReverseImplication = isEncodeReverseImplication
                )
            }
            Method.ExtForestMin -> {
                inferrer.extForestMin(
                    scenarioTree = tree,
                    totalNodes = requireNotNull(maxGuardSize)
                )
            }
            Method.Complete -> {
                inferrer.complete(
                    scenarioTree = tree,
                    negativeScenarioTree = negTree,
                    numberOfStates = requireNotNull(numberOfStates),
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = requireNotNull(maxGuardSize),
                    maxTransitions = maxTransitions,
                    maxTotalGuardsSize = maxTotalGuardsSize
                )
            }
            Method.CompleteMin -> TODO("complete-min method")
            Method.Cegis -> {
                inferrer.cegis(
                    scenarioTree = tree,
                    negativeScenarioTree = negTree,
                    numberOfStates = requireNotNull(numberOfStates),
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = requireNotNull(maxGuardSize),
                    maxTransitions = maxTransitions,
                    maxTotalGuardsSize = maxTotalGuardsSize,
                    smvDir = smvDir
                )
            }
            Method.CegisMin -> {
                inferrer.cegisMin(
                    scenarioTree = tree,
                    initialNegativeScenarioTree = negTree,
                    numberOfStates = numberOfStates,
                    maxGuardSize = maxGuardSize,
                    maxPlateauWidth = maxPlateauWidth,
                    smvDir = smvDir
                )
            }
            Method.ModularBasic -> {
                val modularAutomaton = inferrer.parallelModularBasic(
                    scenarioTree = tree,
                    numberOfModules = requireNotNull(numberOfModules),
                    numberOfStates = requireNotNull(numberOfStates),
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxTransitions = maxTransitions
                )

                if (modularAutomaton == null) {
                    log.failure("Modular automaton not found")
                } else {
                    log.info("Inferred modular automaton, consisting of ${modularAutomaton.modules.values.size} modules:")
                    for ((m, automaton) in modularAutomaton.modules.values.withIndex(start = 1)) {
                        log.info("Automaton #$m has ${automaton.numberOfStates} states and ${automaton.numberOfTransitions} transitions:")
                        automaton.pprint()
                        automaton.dump(outDir, "the_module-$m")
                    }
                    log.info("Output variables controlled by modules:")
                    for (m in 1..numberOfModules!!) {
                        val vars = (1..modularAutomaton.outputNames.size)
                            .filter { z -> modularAutomaton.outputVariableModule[z] == m }
                            .map { z -> modularAutomaton.outputNames[z - 1] }
                        log.info("Module #$m: ${vars.joinToString(" ")}")
                    }
                    modularAutomaton.dumpFbt(
                        outDir.resolve("CentralController.fbt"),
                        name = "CentralController"
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
            Method.ModularBasicMin -> {
                val modularAutomaton = inferrer.parallelModularBasicMin(
                    scenarioTree = tree,
                    numberOfModules = requireNotNull(numberOfModules)
                )

                if (modularAutomaton == null) {
                    log.failure("Modular automaton not found")
                } else {
                    log.info("Inferred modular automaton, consisting of ${modularAutomaton.modules.values.size} modules:")
                    for ((m, automaton) in modularAutomaton.modules.values.withIndex(start = 1)) {
                        log.info("Automaton #$m has ${automaton.numberOfStates} states and ${automaton.numberOfTransitions} transitions:")
                        automaton.pprint()
                        automaton.dump(outDir, "the_module-$m")
                    }
                    log.info("Output variables controlled by modules:")
                    for (m in 1..numberOfModules!!) {
                        val vars = (1..modularAutomaton.outputNames.size)
                            .filter { z -> modularAutomaton.outputVariableModule[z] == m }
                            .map { z -> modularAutomaton.outputNames[z - 1] }
                        log.info("Module #$m: ${vars.joinToString(" ")}")
                    }
                    modularAutomaton.dumpFbt(
                        outDir.resolve("CentralController.fbt"),
                        name = "CentralController"
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
            Method.ConsecutiveModularBasic -> {
                val modularAutomaton = inferrer.consecutiveModularBasic(
                    scenarioTree = tree,
                    numberOfModules = requireNotNull(numberOfModules),
                    numberOfStates = requireNotNull(numberOfStates),
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxTransitions = maxTransitions
                )

                if (modularAutomaton == null) {
                    log.failure("Modular automaton not found")
                } else {
                    log.info("Inferred modular automaton, consisting of ${modularAutomaton.modules.values.size} modules, ${modularAutomaton.numberOfTransitions} transitions:")
                    for ((m, automaton) in modularAutomaton.modules.values.withIndex(start = 1)) {
                        log.info("Automaton #$m has ${automaton.numberOfStates} states and ${automaton.numberOfTransitions} transitions:")
                        automaton.pprint()
                        automaton.dump(outDir, "the_module-$m")
                    }
                    // modularAutomaton.dumpFbt(
                    //     outDir.resolve("CentralController.fbt"),
                    //     name = "CentralController"
                    // )
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
            Method.ConsecutiveModularBasicMin -> {
                val modularAutomaton = inferrer.consecutiveModularBasicMin(
                    scenarioTree = tree,
                    numberOfModules = requireNotNull(numberOfModules)
                )

                if (modularAutomaton == null) {
                    log.failure("Modular automaton not found")
                } else {
                    log.info("Inferred modular automaton, consisting of ${modularAutomaton.modules.values.size} modules, ${modularAutomaton.numberOfTransitions} transitions:")
                    for ((m, automaton) in modularAutomaton.modules.values.withIndex(start = 1)) {
                        log.info("Automaton #$m has ${automaton.numberOfStates} states and ${automaton.numberOfTransitions} transitions:")
                        automaton.pprint()
                        automaton.dump(outDir, "the_module-$m")
                    }
                    // modularAutomaton.dumpFbt(
                    //     outDir.resolve("CentralController.fbt"),
                    //     name = "CentralController"
                    // )
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
            Method.ConsecutiveModularExtended -> {
                val modularAutomaton = inferrer.consecutiveModularExtended(
                    scenarioTree = tree,
                    numberOfModules = requireNotNull(numberOfModules),
                    numberOfStates = requireNotNull(numberOfStates),
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxGuardSize = requireNotNull(maxGuardSize),
                    maxTransitions = maxTransitions,
                    maxTotalGuardsSize = maxTotalGuardsSize,
                    isEncodeReverseImplication = isEncodeReverseImplication
                )

                if (modularAutomaton == null) {
                    log.failure("Modular automaton not found")
                } else {
                    log.info("Inferred modular automaton, consisting of ${modularAutomaton.modules.values.size} modules, ${modularAutomaton.numberOfTransitions} transitions, ${modularAutomaton.totalGuardsSize} nodes:")
                    for ((m, automaton) in modularAutomaton.modules.values.withIndex(start = 1)) {
                        log.info("Automaton #$m has ${automaton.numberOfStates} states, ${automaton.numberOfTransitions} transitions and ${automaton.totalGuardsSize} nodes:")
                        automaton.pprint()
                        automaton.dump(outDir, "the_module-$m")
                    }
                    // modularAutomaton.dumpFbt(
                    //     outDir.resolve("modularAutomaton.fbt"),
                    //     name = "ModularController"
                    // )
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
            Method.ArbitraryModularBasic -> {
                val modularAutomaton = inferrer.arbitraryModularBasic(
                    scenarioTree = tree,
                    numberOfModules = requireNotNull(numberOfModules),
                    numberOfStates = requireNotNull(numberOfStates),
                    maxOutgoingTransitions = maxOutgoingTransitions,
                    maxTransitions = maxTransitions,
                    isEncodeReverseImplication = isEncodeReverseImplication
                )
                modularAutomaton?.minimizeTruthTableGuards(tree)

                if (modularAutomaton == null) {
                    log.failure("Modular automaton not found")
                } else {
                    log.info("Inferred modular automaton, consisting of ${modularAutomaton.modules.values.size} modules, ${modularAutomaton.numberOfTransitions} transitions:")
                    for ((m, automaton) in modularAutomaton.modules.values.withIndex(start = 1)) {
                        log.info("Automaton #$m has ${automaton.numberOfStates} states and ${automaton.numberOfTransitions} transitions:")
                        automaton.pprint()
                        automaton.dump(outDir, "the_module-$m")
                    }
                    modularAutomaton.dumpFbt(
                        outDir.resolve("CentralController.fbt"),
                        name = "CentralController"
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
            Method.ArbitraryModularBasicMin -> {
                val modularAutomaton = inferrer.arbitraryModularBasicMin(
                    scenarioTree = tree,
                    numberOfModules = requireNotNull(numberOfModules)
                )
                modularAutomaton?.minimizeTruthTableGuards(tree)

                if (modularAutomaton == null) {
                    log.failure("Modular automaton not found")
                } else {
                    log.info("Inferred modular automaton, consisting of ${modularAutomaton.modules.values.size} modules, ${modularAutomaton.numberOfTransitions} transitions:")
                    for ((m, automaton) in modularAutomaton.modules.values.withIndex(start = 1)) {
                        log.info("Automaton #$m has ${automaton.numberOfStates} states and ${automaton.numberOfTransitions} transitions:")
                        automaton.pprint()
                        automaton.dump(outDir, "the_module-$m")
                    }
                    modularAutomaton.dumpFbt(
                        outDir.resolve("CentralController.fbt"),
                        name = "CentralController"
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
    log.br(DateTime.nowLocal().format("yyyy-MM-dd HH:mm:ss"))
    val runningTime = measureTime { FbSAT().main(args) }
    log.br(DateTime.nowLocal().format("yyyy-MM-dd HH:mm:ss"))
    log.success("All done in %.3f seconds".format(runningTime.seconds))
}
