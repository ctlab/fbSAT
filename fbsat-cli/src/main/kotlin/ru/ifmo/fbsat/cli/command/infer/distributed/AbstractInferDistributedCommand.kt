package ru.ifmo.fbsat.cli.command.infer.distributed

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.cli.command.infer.AbstractInferCommand
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.utils.MyLogger
import java.io.File

private val logger = MyLogger {}

abstract class AbstractInferDistributedCommand(name: String) :
    AbstractInferCommand<DistributedAutomaton>(name) {

    // Note: use `get()` syntax when overriding those properties!
    protected abstract val numberOfModules: Int
    protected abstract val scenariosFile: File
    protected abstract val inputNames: List<String>
    protected abstract val outputNames: List<String>
    protected abstract val initialOutputValues: OutputValues?
    protected abstract val outDir: File

    protected lateinit var scenarioTree: PositiveScenarioTree private set
    protected lateinit var compoundScenarioTree: PositiveCompoundScenarioTree private set
    protected lateinit var inferrer: Inferrer private set

    final override fun setup() {
        scenarioTree = PositiveScenarioTree.fromFile(
            file = scenariosFile,
            inputNames = inputNames,
            outputNames = outputNames,
            initialOutputValues = initialOutputValues
        )
        scenarioTree.printStats()
        inferrer = Inferrer(solverOptions.solver, outDir)
        val M = numberOfModules
        compoundScenarioTree = PositiveCompoundScenarioTree(
            M = M,
            modularInputEvents = MultiArray.new(M) { scenarioTree.inputEvents },
            modularOutputEvents = MultiArray.new(M) { scenarioTree.outputEvents },
            modularInputNames = MultiArray.new(M) { scenarioTree.inputNames },
            modularOutputNames = MultiArray.new(M) { scenarioTree.outputNames },
            modularInitialOutputValues = MultiArray.new(M) { initialOutputValues }
        )
        for (scenario in scenarioTree.scenarios) {
            val compoundElements = scenario.elements.map { elem ->
                CompoundScenarioElement(MultiArray.new(M) { elem })
            }
            val multiScenario = PositiveCompoundScenario(M, compoundElements)
            compoundScenarioTree.addScenario(multiScenario)
        }
    }

    final override fun printAndCheck(automaton: DistributedAutomaton?) {
        if (automaton == null) {
            logger.error("Automaton not found")
        } else {
            logger.info("Inference: OK")
            // log.info("Inferred automaton:")
            // automaton.pprint()
            // log.info("Inferred automaton has:")
            // automaton.printStats()

            // TODO: automaton.dump(outDir)

            if (automaton.verify(compoundScenarioTree))
                logger.info("Verify: OK")
            else {
                logger.error("Verify: FAILED")
            }
        }
    }
}
