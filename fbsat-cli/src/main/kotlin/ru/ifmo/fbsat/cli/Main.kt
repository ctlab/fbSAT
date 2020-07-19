package ru.ifmo.fbsat.cli

import com.github.ajalt.clikt.core.NoOpCliktCommand
import com.github.ajalt.clikt.core.context
import com.github.ajalt.clikt.core.subcommands
import com.github.ajalt.clikt.output.CliktHelpFormatter
import com.soywiz.klock.DateTime
import com.soywiz.klock.measureTime
import ru.ifmo.fbsat.cli.command.infer.InferCommand
import ru.ifmo.fbsat.core.utils.log

class FbSAT : NoOpCliktCommand(name = "fbsat") {
    init {
        context {
            helpFormatter = CliktHelpFormatter(
                maxWidth = 999,
                requiredOptionMarker = "*",
                showDefaultValues = true,
                showRequiredTag = true
            )
        }
        subcommands(
            InferCommand()
        )
    }
}

fun main(args: Array<String>) {
    log.br(DateTime.nowLocal().format("yyyy-MM-dd HH:mm:ss"))
    val runningTime = measureTime { FbSAT().main(args) }
    log.br(DateTime.nowLocal().format("yyyy-MM-dd HH:mm:ss"))
    log.success("All done in %.3f seconds".format(runningTime.seconds))
}
