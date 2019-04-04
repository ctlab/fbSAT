package ru.ifmo.fbsat.task.extendedce

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.scenario.negative.Counterexample
import ru.ifmo.fbsat.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.negative.toNegativeScenario
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.task.extended.ExtendedTask
import ru.ifmo.fbsat.utils.copyFile
import ru.ifmo.fbsat.utils.log
import java.io.File

class ExtendedCETask(
    val scenarioTree: ScenarioTree,
    initialNegativeScenarioTree: NegativeScenarioTree?,
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int?, // K, K=C if null
    val maxGuardSize: Int, // P
    val outDir: File,
    val smvDir: File,
    private val solverProvider: () -> Solver,
    private val isEncodeAutomaton: Boolean = false,
    private val isEncodeTransitionsOrder: Boolean = false,
    private val loopNumber: Int = 0
) {
    private val negativeScenarioTree =
        initialNegativeScenarioTree
            ?: NegativeScenarioTree.empty(scenarioTree.inputNames, scenarioTree.outputNames)
    // private var algorithmsAssumptions = initialAlgorithmsAssumptions
    private var task = ExtendedTask(
        scenarioTree = scenarioTree,
        negativeScenarioTree = negativeScenarioTree,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions,
        maxGuardSize = maxGuardSize,
        outDir = outDir,
        solver = solverProvider(),
        isEncodeAutomaton = isEncodeAutomaton,
        isEncodeTransitionsOrder = isEncodeTransitionsOrder
    )
    private var _executed = false

    private fun prepareSmvFiles() {
        // Copy smv files to output directory
        for (name in sequenceOf("commands", "extra.smv", "plant.smv", "main.smv", "spec.smv", "Makefile")) {
            val fileSmv = smvDir.resolve(name)
            val fileOut = outDir.resolve(name)
            copyFile(fileSmv, fileOut)
        }
    }

    private fun verifyWithNuSMV(automaton: Automaton): Boolean {
        // Save automaton to smv directory
        automaton.dumpSmv(outDir.resolve("control.smv"))

        // Perform formal verification using NuSMV, generate counterexamples to given ltl-spec
        val cmd = "make model counterexamples"
        log.debug { "Running '$cmd'..." }
        val exitcode = Runtime.getRuntime().exec(cmd, null, outDir).waitFor()
        check(exitcode == 0) { "exitcode $exitcode" }

        // Handle counterexamples after verification
        val fileCounterexamples = outDir.resolve("counterexamples")
        if (fileCounterexamples.exists()) {
            // Read new counterexamples
            val newCounterexamples = Counterexample.fromFile(fileCounterexamples)

            // Convert counterexamples to negative scenarios
            val newNegativeScenarios = newCounterexamples.map {
                it.toNegativeScenario(
                    scenarioTree.inputEvents,
                    scenarioTree.outputEvents,
                    scenarioTree.inputNames,
                    scenarioTree.outputNames
                )
            }

            // Populate negTree with new negative scenarios
            newNegativeScenarios.forEach(negativeScenarioTree::addNegativeScenario)

            // [DEBUG] Append new counterexamples to 'ce'
            log.debug { "Dumping ${newCounterexamples.size} new counterexample(s)..." }
            outDir.resolve("ce").appendText(fileCounterexamples.readText())
            return false
        } else {
            // There is no CEs => automaton is fully-verified
            return true
        }
    }

    fun infer(
        maxTotalGuardsSize: Int? = null
    ): Automaton? {
        check(!_executed) { "The task has already been executed" }
        _executed = true

        prepareSmvFiles()

        var best: Automaton? = null

        // First, try to infer automaton without upper bounding total guards size
        if (maxTotalGuardsSize != null) {
            println("Trying without upper bound (N <= $maxTotalGuardsSize)...")
            if (task.infer(finalize = false) == null) {
                task.finalize()
                error("UNSAT, try larger P or even C")
                // return null
            }
        }

        for (iterationNumber in 1..10000) {
            println("===== Iteration #$iterationNumber =====")

            // Infer automaton
            val automaton = task.infer(maxTotalGuardsSize, finalize = false) ?: break

            log.debug { "Dump intermediate automaton" }
            automaton.dump(outDir, "_automaton_loop${loopNumber}_iter$iterationNumber")

            log.info("Verifying inferred automaton...")
            if (verifyWithNuSMV(automaton)) {
                log.success("There is no counterexamples, nice!")
                best = automaton
                break
            }
        }

        task.finalize()

        return best
    }
}
