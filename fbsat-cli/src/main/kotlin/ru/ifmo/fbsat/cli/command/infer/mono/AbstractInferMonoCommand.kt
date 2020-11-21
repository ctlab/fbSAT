package ru.ifmo.fbsat.cli.command.infer.mono

import ru.ifmo.fbsat.cli.command.infer.AbstractInferCommandWithSetup
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.utils.log

abstract class AbstractInferMonoCommand(name: String) :
    AbstractInferCommandWithSetup<Automaton>(name) {
    final override fun printAndCheck(automaton: Automaton?) {
        if (automaton == null) {
            log.failure("Automaton not found")
        } else {
            log.info("Inferred automaton:")
            automaton.pprint()
            log.info("Inferred automaton has:")
            automaton.printStats()

            automaton.dump(outDir)

            if (automaton.verify(scenarioTree))
                log.success("Verify: OK")
            else {
                log.failure("Verify: FAILED")
            }
        }
    }
}
