package ru.ifmo.fbsat.core.task.complete

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.Counterexample
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.negative.toNegativeScenario
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.utils.copyFile
import ru.ifmo.fbsat.core.utils.log
import java.io.File

interface CompleteCegisTask {
    val scenarioTree: ScenarioTree
    val negativeScenarioTree: NegativeScenarioTree
    val numberOfStates: Int
    val maxOutgoingTransitions: Int
    val maxGuardSize: Int
    val maxTotalGuardsSize: Int?
    val smvDir: File
    val outDir: File

    fun infer(): Automaton?
    fun reuse(newMaxTotalGuardsSize: Int): CompleteCegisTask
    fun update()
    fun finalize2()

    companion object {
        @JvmStatic
        fun create(
            scenarioTree: ScenarioTree,
            negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
            numberOfStates: Int, // C
            maxOutgoingTransitions: Int? = null, // K, K=C if null
            maxGuardSize: Int, // P
            maxTotalGuardsSize: Int? = null, // N, unconstrained if null
            smvDir: File,
            outDir: File,
            solverProvider: () -> Solver,
            autoFinalize: Boolean = true
        ): CompleteCegisTask = CompleteCegisTaskImpl(
            scenarioTree = scenarioTree,
            negativeScenarioTree = negativeScenarioTree
                ?: NegativeScenarioTree(
                    inputEvents = scenarioTree.inputEvents,
                    outputEvents = scenarioTree.outputEvents,
                    inputNames = scenarioTree.inputNames,
                    outputNames = scenarioTree.outputNames
                ),
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions ?: numberOfStates,
            maxGuardSize = maxGuardSize,
            maxTotalGuardsSize = maxTotalGuardsSize,
            smvDir = smvDir,
            outDir = outDir,
            solverProvider = solverProvider,
            autoFinalize = autoFinalize
        )
    }
}

private class CompleteCegisTaskImpl(
    override val scenarioTree: ScenarioTree,
    override val negativeScenarioTree: NegativeScenarioTree,
    override val numberOfStates: Int, // C
    override val maxOutgoingTransitions: Int, // K
    override val maxGuardSize: Int, // P
    override val maxTotalGuardsSize: Int?, // N, unconstrained if null
    override val smvDir: File,
    override val outDir: File,
    private val solverProvider: () -> Solver,
    private val autoFinalize: Boolean
) : CompleteCegisTask {
    @Suppress("JoinDeclarationAndAssignment")
    private val completeTask: CompleteTask
    private var isExecuted = false
    private var isReused = false
    private var isFinalized = false

    init {
        this.completeTask = CompleteTask.create(
            scenarioTree = scenarioTree,
            negativeScenarioTree = negativeScenarioTree,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxGuardSize = maxGuardSize,
            maxTotalGuardsSize = maxTotalGuardsSize,
            outDir = outDir,
            solverProvider = solverProvider,
            autoFinalize = false
        )
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

    override fun infer(): Automaton? {
        check(!isExecuted) { "This task has already been executed. Try using the `reuse()` method." }
        check(!isReused) { "This task has already been reused and can't be executed." }
        check(!isFinalized) { "This task has already been finalized and can't be executed." }
        isExecuted = true

        for (iterationNumber in 1 until 10000) {
            log.info("CEGIS iteration #$iterationNumber")
            // Update reduction to take into account possible negative scenario tree extension
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
            negativeScenarios.forEach(negativeScenarioTree::addNegativeScenario)
        }
        if (autoFinalize) finalize2()
        return null
    }

    override fun reuse(newMaxTotalGuardsSize: Int): CompleteCegisTaskImpl {
        // check(!isReused) { "This task has already been reused." }
        // check(!isFinalized) { "This task has already been finalized and can't be reused." }
        // if (!isExecuted)
        //     log.warn("Reusing the task that has not been executed yet.")
        // isReused = true
        //
        // return CompleteCegisTask(
        //     scenarioTree = this.scenarioTree,
        //     negativeScenarioTree = this.negativeScenarioTree,
        //     numberOfStates = this.numberOfStates,
        //     maxOutgoingTransitions = this.maxOutgoingTransitions,
        //     maxGuardSize = this.maxGuardSize,
        //     maxTotalGuardsSize = newMaxTotalGuardsSize,
        //     outDir = this.outDir,
        //     solver = this.solver,
        //     autoFinalize = this.autoFinalize,
        //     extendedTask = this.extendedTask.reuse(newMaxTotalGuardsSize)
        // )
        TODO()
    }

    override fun update() {
        completeTask.update()
    }

    override fun finalize2() {
        check(!isFinalized) { "This task has already been finalized." }
        isFinalized = true
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
