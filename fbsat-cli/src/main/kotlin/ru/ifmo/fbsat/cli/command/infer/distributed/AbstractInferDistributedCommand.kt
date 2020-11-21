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
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.toImmutable
import java.io.File

abstract class AbstractInferDistributedCommand(name: String) :
    AbstractInferCommand<DistributedAutomaton>(name) {

    // Note: use `get()` syntax when overriding those properties!
    protected abstract val numberOfModules: Int
    protected abstract val scenariosFile: File
    protected abstract val inputNames: List<String>
    protected abstract val outputNames: List<String>
    protected abstract val outDir: File

    protected lateinit var scenarioTree: PositiveScenarioTree private set
    protected lateinit var compoundScenarioTree: PositiveCompoundScenarioTree private set
    protected lateinit var inferrer: Inferrer private set

    final override fun setup() {
        Globals.INITIAL_OUTPUT_VALUES = extraOptions.initialOutputValues ?: OutputValues.zeros(outputNames.size)
        scenarioTree = PositiveScenarioTree.fromFile(scenariosFile, inputNames, outputNames)
        scenarioTree.printStats()
        inferrer = Inferrer(solverOptions.solver, outDir)
        val M = numberOfModules
        compoundScenarioTree = PositiveCompoundScenarioTree(
            M = M,
            modularInputEvents = MultiArray.create(M) { scenarioTree.inputEvents },
            modularOutputEvents = MultiArray.create(M) { scenarioTree.outputEvents },
            modularInputNames = MultiArray.create(M) { scenarioTree.inputNames },
            modularOutputNames = MultiArray.create(M) { scenarioTree.outputNames }
        )
        for (scenario in scenarioTree.scenarios) {
            val compoundElements = scenario.elements.map { elem ->
                CompoundScenarioElement(MultiArray.create(M) { elem }.toImmutable())
            }
            val multiScenario = PositiveCompoundScenario(M, compoundElements)
            compoundScenarioTree.addScenario(multiScenario)
        }
    }

    final override fun printAndCheck(automaton: DistributedAutomaton?) {
        if (automaton == null) {
            log.failure("Automaton not found")
        } else {
            log.success("Inference: OK")
            // log.info("Inferred automaton:")
            // automaton.pprint()
            // log.info("Inferred automaton has:")
            // automaton.printStats()

            // TODO: automaton.dump(outDir)

            if (automaton.verify(compoundScenarioTree))
                log.success("Verify: OK")
            else {
                log.failure("Verify: FAILED")
            }
        }
    }
}
