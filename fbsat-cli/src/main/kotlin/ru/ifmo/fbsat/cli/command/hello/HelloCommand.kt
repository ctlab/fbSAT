package ru.ifmo.fbsat.cli.command.hello

import com.github.ajalt.clikt.core.CliktCommand
import com.github.lipen.satlib.solver.MiniSatSolver
import ru.ifmo.fbsat.cli.command.infer.options.isDebugOption
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.useWith

private val logger = MyLogger {}

class HelloCommand : CliktCommand() {
    private val isDebug: Boolean by isDebugOption()

    override fun run() {
        Globals.IS_DEBUG = isDebug

        logger.debug("Debug!")
        logger.info("Info!")
        logger.warn("Warning!")
        logger.error("Error!")
        val ex = IllegalStateException("Everything OK, just checking exceptions logging")
        logger.delegate.info(ex) { "Check exceptions logging" }

        MiniSatSolver().useWith {
            check(newLiteral() == 1) { "Incorrect new literal" }
            check(newLiteral() == 2) { "Incorrect new literal" }
            check(newLiteral() == 3) { "Incorrect new literal" }

            addClause(1, 2)
            addClause(2, 3)
            addClause(-3)

            check(solve()) { "Unexpected UNSAT" }

            logger.info { "model = ${getModel()}" }
        }
    }
}

private fun main() {
    HelloCommand().main(emptyList())
}
