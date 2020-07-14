package ru.ifmo.fbsat.cli

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.context
import com.github.ajalt.clikt.core.subcommands
import com.github.ajalt.clikt.output.CliktHelpFormatter
import com.soywiz.klock.DateTime
import com.soywiz.klock.measureTime
import ru.ifmo.fbsat.core.utils.log

class FbSAT : CliktCommand() {
    override fun run() = Unit

    init {
        context {
            helpFormatter = CliktHelpFormatter(
                maxWidth = 999,
                requiredOptionMarker = "*",
                showDefaultValues = true,
                showRequiredTag = true
            )
        }
    }
}

fun main(args: Array<String>) {
    log.br(DateTime.nowLocal().format("yyyy-MM-dd HH:mm:ss"))
    val runningTime = measureTime {
        FbSAT().subcommands(Monolithic(), Modular()).main(args)
    }
    log.br(DateTime.nowLocal().format("yyyy-MM-dd HH:mm:ss"))
    log.success("All done in %.3f seconds".format(runningTime.seconds))
}
