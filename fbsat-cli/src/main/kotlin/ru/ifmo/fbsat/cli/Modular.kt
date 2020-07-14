package ru.ifmo.fbsat.cli

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.*
import com.github.ajalt.clikt.parameters.types.choice
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import com.soywiz.klock.DateTime
import ru.ifmo.fbsat.cli.ModularMethod.*
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
import ru.ifmo.fbsat.core.task.modular.basic.parallel.parallelModularBasicMinC
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.consecutiveModularExtended
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.consecutiveModularExtendedMin
import ru.ifmo.fbsat.core.task.modular.extended.parallel.parallelModularExtended
import ru.ifmo.fbsat.core.task.modular.extended.parallel.parallelModularExtendedMin
import ru.ifmo.fbsat.core.utils.*
import java.io.File

class Modular : CliktCommand() {
    private val fileScenarios: File by option(
        "-i", "--scenarios",
        help = "File with scenarios",
        metavar = "<path>"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    ).required()

    private val outDir: File by option(
        "-o", "--outdir",
        help = "Output directory",
        metavar = "<path>"
    ).file().defaultLazy {
        val now = DateTime.nowLocal().format("yyyy-MM-dd_HH-mm-ss")
        File("out/$now")
    }

    private val fileInputNames: File? by option(
        "--input-names",
        help = "File with input names [defaults to PnP names]"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    )

    private val fileOutputNames: File? by option(
        "--output-names",
        help = "File with output names [defaults to PnP names]"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    )

    private val method: ModularMethod by option(
        "-m", "--method",
        help = "Method to use",
        metavar = "<method>"
    ).choice(
        values().associateBy { it.s }
    ).required()

    private val numberOfStates: Int? by option(
        "-C",
        help = "Number of automaton states",
        metavar = "<int>"
    ).int()

    private val maxOutgoingTransitions: Int? by option(
        "-K",
        help = "Maximum number of transitions from each state",
        metavar = "<int>"
    ).int()

    private val maxGuardSize: Int? by option(
        "-P",
        help = "Maximum guard size (number of parse tree nodes)",
        metavar = "<int>"
    ).int()

    private val maxTransitions: Int? by option(
        "-T",
        help = "Upper bound for the total number of transitions",
        metavar = "<int>"
    ).int()

    private val maxTotalGuardsSize: Int? by option(
        "-N",
        help = "Upper bound for the total size of guards",
        metavar = "<int>"
    ).int()

    private val numberOfModules: Int? by option(
        "-M",
        help = "Number of modules",
        metavar = "<int>"
    ).int()

    private val solverCmd: String by option(
        "--solver",
        help = "SAT-solver for FileSolver backend (use %s placeholder for filename)",
        metavar = "<cmd>"
    ).default(
        "cadical %s"
    )

    private val solverBackend: SolverBackend by option(
        help = "Solver backend (default: IncrementalSolver)"
    ).switch(
        "--icms" to SolverBackend.INCREMENTAL_CRYPTOMINISAT,
        "--filesolver" to SolverBackend.FILE,
        "--minisat" to SolverBackend.MINISAT,
        "--cadical" to SolverBackend.CADICAL
    ).default(
        SolverBackend.INCREMENTAL_CRYPTOMINISAT
    )

    private val isForbidOr: Boolean by option(
        "--forbid-or"
    ).flag(
        "--allow-or",
        default = Globals.IS_FORBID_OR
    )

    private val isForbidTransitionsToFirstState: Boolean by option(
        "--forbid-transitions-to-first-state"
    ).flag(
        "--allow-transitions-to-first-state",
        default = Globals.IS_FORBID_TRANSITIONS_TO_FIRST_STATE
    )

    private val isBfsAutomaton: Boolean by option(
        "--bfs-automaton"
    ).flag(
        "--no-bfs-automaton",
        default = Globals.IS_BFS_AUTOMATON
    )

    private val isBfsGuard: Boolean by option(
        "--bfs-guard"
    ).flag(
        "--no-bfs-guard",
        default = Globals.IS_BFS_GUARD
    )

    private val isOnlyC: Boolean by option(
        "--only-C",
        help = "[basic-min] Minimize only C, without T"
    ).flag()

    private val fileVis: File? by option(
        "--vis",
        help = "[DEBUG] Visualize given counterexamples via graphviz"
    ).file(
        mustExist = true,
        canBeDir = false,
        mustBeReadable = true
    )

    private val initialOutputValues: OutputValues? by option(
        "--initial-output-values",
        help = "Initial output values (as a bitstring)",
        metavar = "<[01]+>"
    ).convert {
        require(it.matches(Regex("[01]+"))) {
            "--initial-output-values must match [01]+"
        }
        OutputValues(it.map { c -> c == '1' })
    }

    private val epsilonOutputEvents: EpsilonOutputEvents by option(
        "--epsilon-output-events",
        help = "Epsilon output events"
    ).choice(
        "start" to EpsilonOutputEvents.START,
        "onlystart" to EpsilonOutputEvents.ONLYSTART,
        "none" to EpsilonOutputEvents.NONE
    ).default(
        Globals.EPSILON_OUTPUT_EVENTS
    )

    private val startStateAlgorithms: StartStateAlgorithms by option(
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

    private val isEncodeReverseImplication: Boolean by option(
        "--encode-reverse-implication",
        help = "Encode reverse implication"
    ).flag(
        "--no-encode-reverse-implication",
        default = true
    )

    private val isEncodeTransitionsOrder: Boolean by option(
        "--encode-transitions-order",
        help = "[DEBUG] Encode transitions lexicographic order"
    ).flag(
        "--no-encode-transitions-order",
        default = Globals.IS_ENCODE_TRANSITIONS_ORDER
    )

    private val isEncodeTerminalsOrder: Boolean by option(
        "--encode-terminals-order",
        help = "[DEBUG] Encode terminal numbers lexicographic order"
    ).flag(
        "--no-encode-terminals-order",
        default = Globals.IS_ENCODE_TERMINALS_ORDER
    )

    private val isEncodeTerminalsMiniOrder: Boolean by option(
        "--encode-terminals-mini-order",
        help = "[DEBUG] Encode AND/OR children-terminals order"
    ).flag(
        "--no-encode-terminals-mini-order",
        default = Globals.IS_ENCODE_TERMINALS_MINI_ORDER
    )

    private val isEncodeHardToExplain: Boolean by option(
        "--encode-hard-to-explain",
        help = "[DEBUG] Encode some hard to explain thing"
    ).flag(
        "--no-encode-hard-to-explain",
        default = Globals.IS_ENCODE_HARD_TO_EXPLAIN
    )

    private val isEncodeTotalizer: Boolean by option(
        "--encode-totalizer",
        help = "Encode totalizer when upper bound is null"
    ).flag(
        "--no-encode-totalizer",
        default = Globals.IS_ENCODE_TOTALIZER
    )

    private val isEncodeDisjunctiveTransitions: Boolean by option(
        "--encode-disjunctive-transitions",
        help = "Encode disjunctive transitions (adhocly forbid priority function)"
    ).flag(
        "--no-encode-disjunctive-transitions",
        default = Globals.IS_ENCODE_DISJUNCTIVE_TRANSITIONS
    )

    private val isReuseK: Boolean by option(
        "--reuse-k",
        help = "Reuse K found by ExtendedMinTask during CEGIS"
    ).flag(
        "--no-reuse-k",
        default = Globals.IS_REUSE_K
    )

    private val isDumpVarsInCnf: Boolean by option(
        "--dump-vars-in-cnf",
        help = "Dump variables in CNF"
    ).flag(
        "--no-dump-vars-in-cnf",
        default = Globals.IS_DUMP_VARS_IN_CNF
    )

    private val isDebug: Boolean by option(
        "--debug",
        help = "Debug mode"
    ).flag(
        default = Globals.IS_DEBUG
    )

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
        Globals.IS_DUMP_VARS_IN_CNF = isDumpVarsInCnf
        Globals.IS_DEBUG = isDebug

        // outDir.deleteRecursively()
        // outDir.walkBottomUp().forEach { if (it != outDir) it.delete() }
        outDir.mkdirs()
        check(outDir.exists()) { "Output directory does not exist" }

        val inputNames = fileInputNames?.readLines() ?: inputNamesPnP
        val outputNames = fileOutputNames?.readLines() ?: outputNamesPnP
        log.info("Input names: $inputNames")
        log.info("Output names: $outputNames")

        Globals.INITIAL_OUTPUT_VALUES = initialOutputValues ?: OutputValues.zeros(outputNames.size)
        check(Globals.INITIAL_OUTPUT_VALUES.values.size == outputNames.size) {
            "Initial values size must be equal to the number of output variables"
        }

        val tree = ScenarioTree.fromFile(fileScenarios, inputNames, outputNames)
        log.info("Scenarios: ${tree.scenarios.size}")
        log.info("Elements: ${tree.scenarios.sumBy { it.elements.size }}")
        log.info("Scenario tree size: ${tree.size}")

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
            SolverBackend.INCREMENTAL_CRYPTOMINISAT -> {
                { Solver.icms() }
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

        when (method) {
            ParallelBasic -> {
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
            }
            ParallelBasicMin -> {
                val modularAutomaton =
                    if (isOnlyC)
                        inferrer.parallelModularBasicMinC(
                            scenarioTree = tree,
                            numberOfModules = requireNotNull(numberOfModules)
                        )
                    else
                        inferrer.parallelModularBasicMin(
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
            }
            ParallelExtended -> {
                val modularAutomaton = inferrer.parallelModularExtended(
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
            }
            ParallelExtendedMin -> {
                val modularAutomaton = inferrer.parallelModularExtendedMin(
                    scenarioTree = tree,
                    numberOfModules = requireNotNull(numberOfModules),
                    numberOfStates = numberOfStates,
                    maxGuardSize = requireNotNull(maxGuardSize)
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
            }
            ConsecutiveBasic -> {
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
            }
            ConsecutiveBasicMin -> {
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
            }
            ConsecutiveExtended -> {
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
            }
            ConsecutiveExtendedMin -> {
                val modularAutomaton = inferrer.consecutiveModularExtendedMin(
                    scenarioTree = tree,
                    numberOfModules = requireNotNull(numberOfModules),
                    numberOfStates = numberOfStates,
                    maxGuardSize = requireNotNull(maxGuardSize)
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
            }
            ArbitraryBasic -> {
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
            }
            ArbitraryBasicMin -> {
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
            }
            else -> TODO("method '$method'")
        }
    }
}
