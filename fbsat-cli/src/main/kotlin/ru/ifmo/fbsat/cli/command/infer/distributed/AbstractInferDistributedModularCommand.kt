package ru.ifmo.fbsat.cli.command.infer.distributed

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.cli.command.infer.AbstractInferCommand
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import java.io.File

private val logger = MyLogger {}

abstract class AbstractInferDistributedModularCommand(name: String) :
    AbstractInferCommand<DistributedAutomaton>(name) {

    // Note: use `get()` or `by lazy` syntax when overriding those properties!
    protected abstract val numberOfModules: Int
    protected abstract val modularScenariosFile: MultiArray<File>
    protected abstract val modularInputNames: MultiArray<List<String>>
    protected abstract val modularOutputNames: MultiArray<List<String>>
    protected abstract val outDir: File

    // protected lateinit var compoundScenarioTree: PositiveCompoundScenarioTree private set
    protected lateinit var modularScenarioTree: MultiArray<PositiveScenarioTree> private set
    protected lateinit var inferrer: Inferrer private set

    final override fun setup() {
        val M = numberOfModules
        require(modularScenariosFile.shape.single() == M)
        require(modularInputNames.shape.single() == M)
        require(modularOutputNames.shape.single() == M)

        Globals.INITIAL_OUTPUT_VALUES = extraOptions.initialOutputValues ?: OutputValues.zeros(M)

        modularScenarioTree = MultiArray.new(M) { (m) ->
            PositiveScenarioTree.fromFile(
                modularScenariosFile[m],
                modularInputNames[m],
                modularOutputNames[m]
            ).also {
                logger.info("= modularScenarioTree[$m]:")
                it.printStats()
            }
        }

        inferrer = Inferrer(solverOptions.solver, outDir)
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

            if (automaton.verify(modularScenarioTree))
                logger.info("Verify: OK")
            else {
                logger.error("Verify: FAILED")
            }
        }
    }
}
