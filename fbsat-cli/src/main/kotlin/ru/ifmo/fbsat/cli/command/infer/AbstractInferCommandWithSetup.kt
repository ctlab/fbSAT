package ru.ifmo.fbsat.cli.command.infer

import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.utils.Globals
import java.io.File

abstract class AbstractInferCommandWithSetup<AutomatonType : Any>(name: String) :
    AbstractInferCommand<AutomatonType>(name) {
    // Note: use `get()` syntax when overriding those properties!
    protected abstract val scenariosFile: File
    protected abstract val inputNames: List<String>
    protected abstract val outputNames: List<String>
    protected abstract val outDir: File

    protected lateinit var scenarioTree: PositiveScenarioTree private set
    protected lateinit var inferrer: Inferrer private set

    final override fun setup() {
        outDir.mkdirs()
        check(outDir.exists()) { "Output directory does not exist" }

        Globals.INITIAL_OUTPUT_VALUES = extraOptions.initialOutputValues ?: OutputValues.zeros(outputNames.size)
        scenarioTree = PositiveScenarioTree.fromFile(scenariosFile, inputNames, outputNames)
        scenarioTree.printStats()

        inferrer = Inferrer(solverOptions.solver, outDir)

        setup2()
    }

    open fun setup2() {}
}
