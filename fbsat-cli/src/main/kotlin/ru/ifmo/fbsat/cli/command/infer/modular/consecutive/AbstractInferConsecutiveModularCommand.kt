package ru.ifmo.fbsat.cli.command.infer.modular.consecutive

import ru.ifmo.fbsat.cli.command.infer.AbstractInferCommandWithSetup
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.toOld
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.withIndex

abstract class AbstractInferConsecutiveModularCommand(name: String) :
    AbstractInferCommandWithSetup<ConsecutiveModularAutomaton>(name) {
    final override fun printAndCheck(automaton: ConsecutiveModularAutomaton?) {
        if (automaton == null) {
            log.failure("Consecutive modular automaton not found")
        } else {
            log.info("Inferred consecutive modular automaton consists of ${automaton.modules.shape.single()} modules:")
            for ((m, module) in automaton.modules.values.withIndex(start = 1)) {
                log.info("Module #$m (${module.getStats()}):")
                module.pprint()
            }
            log.info("Inferred consecutive modular automaton has:")
            automaton.printStats()

            // TODO: automaton.dump(outDir)

            if (automaton.verify(scenarioTree.toOld()))
                log.success("Verify: OK")
            else {
                log.failure("Verify: FAILED")
            }
        }
    }
}
