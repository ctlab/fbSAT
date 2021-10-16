package ru.ifmo.fbsat.cli.command.infer

import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import java.io.File

private val logger = MyLogger {}

abstract class AbstractInferCommandWithSetup<AutomatonType : Any>(name: String) :
    AbstractInferCommand<AutomatonType>(name) {

    // Note: use `get()` (or `by lazy`) syntax when overriding those properties!
    protected abstract val scenariosFile: File
    protected abstract val inputNames: List<String>
    protected abstract val outputNames: List<String>
    protected abstract val initialOutputValues: OutputValues?
    protected abstract val outDir: File

    protected lateinit var scenarioTree: PositiveScenarioTree private set
    protected lateinit var inferrer: Inferrer private set

    final override fun setup() {
        outDir.mkdirs()
        check(outDir.exists()) { "Output directory does not exist" }

        if (!Globals.IS_ENCODE_EVENTLESS) {
            if (Globals.IS_ENCODE_TRANSITION_FUNCTION) {
                logger.warn("IS_ENCODE_TRANSITION_FUNCTION is true, but IS_ENCODE_EVENTLESS is false, so the former has no effect")
            }
            if (Globals.IS_ENCODE_EPSILON_PASSIVE) {
                logger.warn("IS_ENCODE_EPSILON_PASSIVE is true, but IS_ENCODE_EVENTLESS is false, so the former has no effect")
            }
            if (Globals.IS_ENCODE_NOT_EPSILON_ACTIVE) {
                logger.warn("IS_ENCODE_NOT_EPSILON_ACTIVE is true, but IS_ENCODE_EVENTLESS is false, so the former has no effect")
            }
            if (Globals.IS_FIX_ACTIVE) {
                logger.warn("IS_FIX_ACTIVE is true, but IS_ENCODE_EVENTLESS is false, so the former has no effect")
            }
        }

        scenarioTree = PositiveScenarioTree.fromFile(
            file = scenariosFile,
            inputNames = inputNames,
            outputNames = outputNames,
            initialOutputValues = initialOutputValues
        )
        scenarioTree.printStats()

        inferrer = Inferrer(solverOptions.solver, outDir)

        setup2()
    }

    open fun setup2() {}
}
