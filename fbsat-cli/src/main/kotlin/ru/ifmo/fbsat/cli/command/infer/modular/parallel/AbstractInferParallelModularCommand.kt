package ru.ifmo.fbsat.cli.command.infer.modular.parallel

import ru.ifmo.fbsat.cli.command.infer.AbstractInferCommandWithSetup
import ru.ifmo.fbsat.core.automaton.ParallelModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.toOld
import ru.ifmo.fbsat.core.utils.mylog
import ru.ifmo.fbsat.core.utils.withIndex

abstract class AbstractInferParallelModularCommand(name: String) :
    AbstractInferCommandWithSetup<ParallelModularAutomaton>(name) {
    final override fun printAndCheck(automaton: ParallelModularAutomaton?) {
        if (automaton == null) {
            mylog.failure("Parallel modular automaton not found")
        } else {
            mylog.info("Inferred parallel modular automaton consists of ${automaton.modules.shape.single()} modules:")
            for ((m, module) in automaton.modules.values.withIndex(start = 1)) {
                mylog.info("Module #$m (${module.getStats()}):")
                module.pprint()
            }
            mylog.info("Inferred parallel modular automaton has:")
            automaton.printStats()

            // TODO: print controlled variables of each module

            // TODO: automaton.dump(outDir)

            if (automaton.verify(scenarioTree.toOld()))
                mylog.success("Verify: OK")
            else {
                mylog.failure("Verify: FAILED")
            }
        }
    }
}
