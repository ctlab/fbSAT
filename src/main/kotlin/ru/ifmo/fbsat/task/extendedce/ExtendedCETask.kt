package ru.ifmo.fbsat.task.extendedce

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.negative.Counterexample
import ru.ifmo.fbsat.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.negative.toNegativeScenario
import ru.ifmo.fbsat.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.task.extended.ExtendedTask
import java.io.File

class ExtendedCETask(
    val scenarioTree: ScenarioTree,
    initialNegativeScenarioTree: NegativeScenarioTree?,
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int?, // K, K=C if null
    val maxGuardSize: Int, // P
    val solverProvider: () -> Solver,
    val smvDir: File,
    val isEncodeAutomaton: Boolean = false,
    val isEncodeTransitionsOrder: Boolean = false
) {
    private val negativeScenarioTree =
        initialNegativeScenarioTree
            ?: NegativeScenarioTree.empty(scenarioTree.inputNames, scenarioTree.outputNames)
    // private var algorithmsAssumptions = initialAlgorithmsAssumptions
    private var task = ExtendedTask(
        scenarioTree,
        negativeScenarioTree,
        numberOfStates,
        maxOutgoingTransitions,
        maxGuardSize,
        solverProvider,
        isEncodeAutomaton = isEncodeAutomaton,
        isEncodeTransitionsOrder = isEncodeTransitionsOrder
    )
    private var _executed = false

    fun infer(
        maxTotalGuardsSize: Int? = null
    ): Automaton? {
        check(!_executed) { "The task has already been executed" }
        _executed = true

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

            // Save automaton to smv directory
            automaton.dumpSmv(smvDir.resolve("control.smv"))

            // Perform formal verification using NuSMV, generate counterexamples to given ltl-spec
            val cmd = "make model counterexamples"
            println("[$] Running '$cmd'...")
            Runtime.getRuntime().exec(cmd, null, smvDir).waitFor()

            // Handle counterexamples after verification
            val fileCounterexamples = smvDir.resolve("counterexamples")
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
                println("[*] Appending ${newCounterexamples.size} new counterexample(s) to 'ce'...")
                File("ce").appendText(fileCounterexamples.readText())
            } else {
                // There is no CEs => automaton is fully-verified
                println("[+] There is no counterexamples, nice!")
                best = automaton
                break
            }
        }

        task.finalize()

        return best
    }
}
