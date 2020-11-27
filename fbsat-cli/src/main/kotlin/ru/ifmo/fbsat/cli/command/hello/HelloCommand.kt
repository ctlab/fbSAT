package ru.ifmo.fbsat.cli.command.hello

import com.github.ajalt.clikt.core.CliktCommand
import com.github.lipen.satlib.solver.MiniSatSolver
import ru.ifmo.fbsat.core.utils.useWith

private val log = mu.KotlinLogging.logger {}

class HelloCommand : CliktCommand() {
    override fun run() {
        log.debug { "Hello" }
        log.warn { "Warning!" }
        log.error { "Error!" }
        log.info(IllegalStateException("Everything OK, just checking exceptions logging")) {
            "Check exceptions logging"
        }

        MiniSatSolver().useWith {
            check(newLiteral() == 1)
            check(newLiteral() == 2)
            check(newLiteral() == 3)

            addClause(1, 2)
            addClause(2, 3)
            addClause(-3)

            check(solve())

            log.info { "model = ${getModel()}" }
        }
    }
}
