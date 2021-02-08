package ru.ifmo.fbsat.cli.command.infer.modular.arbitrary

import ru.ifmo.fbsat.cli.command.infer.AbstractInferCommandWithSetup
import ru.ifmo.fbsat.core.automaton.ArbitraryModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.toOld
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.withIndex

private val logger = MyLogger {}

abstract class AbstractInferArbitraryModularCommand(name: String) :
    AbstractInferCommandWithSetup<ArbitraryModularAutomaton>(name) {

    final override fun printAndCheck(automaton: ArbitraryModularAutomaton?) {
        if (automaton == null) {
            logger.error("Arbitrary modular automaton not found")
        } else {
            logger.info("Inferred arbitrary modular automaton consists of ${automaton.modules.shape.single()} modules:")
            for ((m, module) in automaton.modules.values.withIndex(start = 1)) {
                logger.info("Module #$m (${module.getStats()}):")
                module.pprint()
            }
            logger.info("Inferred arbitrary modular automaton has:")
            automaton.printStats()

            automaton.dump(outDir)

            if (automaton.verify(scenarioTree.toOld()))
                logger.info("Verify: OK")
            else {
                logger.error("Verify: FAILED")
            }
        }
    }
}
