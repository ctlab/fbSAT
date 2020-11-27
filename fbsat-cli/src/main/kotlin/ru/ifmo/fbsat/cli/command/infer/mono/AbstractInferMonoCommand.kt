package ru.ifmo.fbsat.cli.command.infer.mono

import ru.ifmo.fbsat.cli.command.infer.AbstractInferCommandWithSetup
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.utils.mylog

abstract class AbstractInferMonoCommand(name: String) :
    AbstractInferCommandWithSetup<Automaton>(name) {
    final override fun printAndCheck(automaton: Automaton?) {
        if (automaton == null) {
            mylog.failure("Automaton not found")
        } else {
            mylog.info("Inferred automaton:")
            automaton.pprint()
            mylog.info("Inferred automaton has:")
            automaton.printStats()

            automaton.dump(outDir)

            if (automaton.verify(scenarioTree))
                mylog.success("Verify: OK")
            else {
                mylog.failure("Verify: FAILED")
            }
        }
    }
}
