package ru.ifmo.fbsat.core.task.complete.single

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.Counterexample
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.negative.toNegativeScenario
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.utils.copyFile
import ru.ifmo.fbsat.core.utils.log
import java.io.File

class CompleteCegisTask(
    scenarioTree: ScenarioTree,
    negativeScenarioTree: NegativeScenarioTree? = null,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    val smvDir: File,
    val outDir: File,
    val solver: Solver,
    val autoFinalize: Boolean = true
) {
    private val completeTask = CompleteTask(
        scenarioTree = scenarioTree,
        negativeScenarioTree = negativeScenarioTree,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions,
        maxGuardSize = maxGuardSize,
        maxTotalGuardsSize = maxTotalGuardsSize,
        outDir = outDir,
        solver = solver,
        autoFinalize = false
    )

    init {
        prepareSmvFiles()
    }

    private fun prepareSmvFiles() {
        // Copy smv files to output directory
        for (name in sequenceOf("commands", "extra.smv", "plant.smv", "main.smv", "spec.smv", "Makefile")) {
            val fileSmv = smvDir.resolve(name)
            val fileOut = outDir.resolve(name)
            copyFile(fileSmv, fileOut)
        }
    }

    fun infer(): Automaton? {
        val scenarioTree = completeTask.vars.scenarioTree
        val negativeScenarioTree = completeTask.vars.negativeScenarioTree
        lateinit var lastNegativeScenarios: List<NegativeScenario>

        for (iterationNumber in 1 until 10000) {
            log.info("CEGIS iteration #$iterationNumber")
            // Update to take into account possible extension of the negative scenario tree
            update()
            // Infer update
            val automaton: Automaton? = completeTask.infer()
            if (automaton == null) {
                if (autoFinalize) finalize2()
                return null
            }
            // ==============
            // Dump intermediate automaton
            automaton.dump(outDir, "_automaton_iter%04d".format(iterationNumber))
            // ==============
            // Verify automaton with NuSMV
            val counterexamples = automaton.verifyWithNuSMV(outDir)
            if (counterexamples.isEmpty()) {
                log.success("These is no counterexamples, nice!")
                if (autoFinalize) finalize2()
                return automaton
            }
            // Convert counterexamples to negative scenarios
            val negativeScenarios = counterexamples.map {
                it.toNegativeScenario(
                    scenarioTree.inputEvents,
                    scenarioTree.outputEvents,
                    scenarioTree.inputNames,
                    scenarioTree.outputNames
                )
            }
            // Populate negTree with new negative scenarios
            val treeSize = negativeScenarioTree.size
            negativeScenarios.forEach(negativeScenarioTree::addNegativeScenario)
            val treeSizeDiff = negativeScenarioTree.size - treeSize
            // Note: it is suffice to check just `negSc == lastNegSc`, but it may be costy,
            // so check it only in a specific case - when negative tree does not change its size
            if (treeSizeDiff == 0 && negativeScenarios == lastNegativeScenarios)
                error("Stale")
            lastNegativeScenarios = negativeScenarios
        }
        if (autoFinalize) finalize2()
        return null
    }

    fun update() {
        completeTask.update()
    }

    fun finalize2() {
        completeTask.finalize2()
    }
}

fun Automaton.verifyWithNuSMV(dir: File): List<Counterexample> {
    // Save automaton to smv directory
    dumpSmv(dir.resolve("control.smv"))

    // Perform formal verification using NuSMV, generate counterexamples to given ltl-spec
    val cmd = "make clean model counterexamples"
    log.debug { "Running '$cmd'..." }
    val exitcode = Runtime.getRuntime().exec(cmd, null, dir).waitFor()
    check(exitcode == 0) { "NuSMV exitcode: $exitcode" }

    // Handle counterexamples after verification
    val fileCounterexamples = dir.resolve("counterexamples")
    return if (fileCounterexamples.exists()) {
        // Read new counterexamples
        val counterexamples: List<Counterexample> = Counterexample.fromFile(fileCounterexamples)

        // [DEBUG] Append new counterexamples to 'ce'
        log.debug { "Dumping ${counterexamples.size} counterexample(s)..." }
        dir.resolve("ce").appendText(fileCounterexamples.readText())

        counterexamples
    } else {
        emptyList()
    }
}
