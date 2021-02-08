package ru.ifmo.fbsat.cli

import com.github.ajalt.clikt.core.NoOpCliktCommand
import com.github.ajalt.clikt.core.context
import com.github.ajalt.clikt.core.subcommands
import com.github.ajalt.clikt.output.CliktHelpFormatter
import com.soywiz.klock.DateTime
import com.soywiz.klock.measureTime
import ru.ifmo.fbsat.cli.command.hello.HelloCommand
import ru.ifmo.fbsat.cli.command.infer.InferCommand
import ru.ifmo.fbsat.core.utils.MyLogger

private val logger = MyLogger {}

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
            InferCommand(),
            HelloCommand(),
        )
    }
}

fun main(args: Array<String>) {
    val dateTimeFormat = "yyyy-MM-dd HH:mm:ss"
    logger.info("Start time: ${DateTime.nowLocal().format(dateTimeFormat)}")
    logger.info("Args: ./fbsat " + args.joinToString(" ") {
        if (it.contains(" ")) "\"$it\"" else it
    })
    val runningTime = measureTime { FbSAT().main(args) }
    logger.info("End time: ${DateTime.nowLocal().format(dateTimeFormat)}")
    logger.info("All done in %.3f seconds".format(runningTime.seconds))
}
