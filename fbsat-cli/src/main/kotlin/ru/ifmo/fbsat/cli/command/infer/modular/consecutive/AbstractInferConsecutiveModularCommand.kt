package ru.ifmo.fbsat.cli.command.infer.modular.consecutive

import ru.ifmo.fbsat.cli.command.infer.AbstractInferCommandWithSetup
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.toOld
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.withIndex

private val logger = MyLogger {}

abstract class AbstractInferConsecutiveModularCommand(name: String) :
    AbstractInferCommandWithSetup<ConsecutiveModularAutomaton>(name) {
    final override fun printAndCheck(automaton: ConsecutiveModularAutomaton?) {
        if (automaton == null) {
            logger.error("Consecutive modular automaton not found")
        } else {
            logger.info("Inferred consecutive modular automaton consists of ${automaton.modules.shape.single()} modules:")
            for ((m, module) in automaton.modules.values.withIndex(start = 1)) {
                logger.info("Module #$m (${module.getStats()}):")
                module.pprint()
            }
            logger.info("Inferred consecutive modular automaton has:")
            automaton.printStats()

            // TODO: automaton.dump(outDir)

            if (automaton.verify(scenarioTree.toOld()))
                logger.info("Verify: OK")
            else {
                logger.error("Verify: FAILED")
            }
        }
    }
}
