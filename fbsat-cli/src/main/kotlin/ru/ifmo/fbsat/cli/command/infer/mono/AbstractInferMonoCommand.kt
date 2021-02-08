package ru.ifmo.fbsat.cli.command.infer.mono

import ru.ifmo.fbsat.cli.command.infer.AbstractInferCommandWithSetup
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.utils.MyLogger

private val logger = MyLogger {}

abstract class AbstractInferMonoCommand(name: String) :
    AbstractInferCommandWithSetup<Automaton>(name) {

    final override fun printAndCheck(automaton: Automaton?) {
        if (automaton == null) {
            logger.error("Automaton not found")
        } else {
            logger.info("Inferred automaton:")
            automaton.pprint()
            logger.info("Inferred automaton has:")
            automaton.printStats()

            automaton.dump(outDir)

            if (automaton.verify(scenarioTree))
                logger.info("Verify: OK")
            else {
                logger.error("Verify: FAILED")
            }
        }
    }
}
