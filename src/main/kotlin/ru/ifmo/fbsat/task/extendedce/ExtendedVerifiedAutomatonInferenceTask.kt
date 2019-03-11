package ru.ifmo.fbsat.task.extendedce

import ru.ifmo.fbsat.automaton.Automaton
import ru.ifmo.fbsat.scenario.Counterexample
import ru.ifmo.fbsat.scenario.NegativeScenarioTree
import ru.ifmo.fbsat.scenario.ScenarioTree
import ru.ifmo.fbsat.scenario.toNegativeScenario
import ru.ifmo.fbsat.solver.Solver
import ru.ifmo.fbsat.task.extended.ExtendedAutomatonInferenceTask
import java.io.File

class ExtendedVerifiedAutomatonInferenceTask(
    val scenarioTree: ScenarioTree,
    val initialNegativeScenarioTree: NegativeScenarioTree?,
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int?, // K, K=C if null
    val maxGuardSize: Int, // P
    val solverProvider: () -> Solver,
    val smvDir: File,
    private val isEncodeAutomaton: Boolean = false
) {
    fun infer(maxTotalGuardsSize: Int? = null, finalize: Boolean = true): Automaton? {
        val negativeScenarioTree = initialNegativeScenarioTree
            ?: NegativeScenarioTree(
                emptyList(),
                inputNames = scenarioTree.inputNames,
                outputNames = scenarioTree.outputNames
            )

        val task = ExtendedAutomatonInferenceTask(
            scenarioTree,
            negativeScenarioTree,
            numberOfStates,
            maxOutgoingTransitions,
            maxGuardSize,
            solverProvider,
            isEncodeAutomaton = isEncodeAutomaton
        )
        var automaton: Automaton? = null

        for (iterationNumber in 1..10000) {
            println("===== Iteration #$iterationNumber =====")

            // Infer automaton
            automaton = task.infer(maxTotalGuardsSize, finalize = false) ?: return null

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

                // Populate CETree with new negative scenarios
                newNegativeScenarios.forEach(negativeScenarioTree::addNegativeScenario)

                // [DEBUG] Append new counterexamples to 'ce'
                println("[*] Appending new counterexamples to 'ce'...")
                File("ce").appendText(fileCounterexamples.readText())
                // val cmd2 = "cat $fileCounterexamples >> ce"
                // println("[$] Running '$cmd2'...")
                // Runtime.getRuntime().exec(cmd2).waitFor()
            } else {
                // There is not CEs => automaton is fully-verified
                println("[+] There is no counterexamples, nice!")
                break
            }
        }

        if (finalize)
            task.finalize()

        return automaton
    }
}
