package ru.ifmo.fbsat.cli

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.context
import com.github.ajalt.clikt.core.subcommands
import com.github.ajalt.clikt.output.CliktHelpFormatter
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.types.file
import com.soywiz.klock.DateTime
import com.soywiz.klock.measureTime
import ru.ifmo.fbsat.cli.command.hello.HelloCommand
import ru.ifmo.fbsat.cli.command.infer.InferCommand
import ru.ifmo.fbsat.cli.command.randexp.BatchRandomExperimentCommand
import ru.ifmo.fbsat.cli.command.randexp.MultiRandomExperimentCommand
import ru.ifmo.fbsat.cli.command.randexp.RandomExperimentCommand
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.setupLogFileAppender
import ru.ifmo.fbsat.core.utils.Timing
import java.io.File

private val logger = MyLogger {}

private const val dateTimeFormat = "yyyy-MM-dd HH:mm:ss"

class FbSAT(
    private val args: Array<String> = arrayOf("?"),
) : CliktCommand(name = "fbsat") {

    private val isLog: Boolean by option(
        "--log",
        help = "Write logs to --log-file"
    ).flag(
        "--no-log",
        default = true
    )
    private val logFile: File by option(
        "--log-file",
        help = "Path to log file",
        metavar = "<path>"
    ).file(
        canBeDir = false,
    ).default(File("logs/fbsat.log"))
    private val logPattern: String by option(
        "--log-pattern",
        help = "Pattern for logging",
        metavar = "<log4j pattern>",
        hidden = true
    ).default("[%date{yyyy-MM-dd HH:mm:ss}] [%level] %logger: %message%n%throwable")

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
            RandomExperimentCommand(),
            MultiRandomExperimentCommand(),
            BatchRandomExperimentCommand(),
        )
    }

    override fun run() {
        // Setup log4j logging to the specified file
        if (isLog) {
            setupLogFileAppender(
                path = logFile.path,
                pattern = logPattern
            )
        }

        logger.info("Start time: ${DateTime.nowLocal().format(dateTimeFormat)}")
        logger.info("Args: ./fbsat " + args.joinToString(" ") {
            if (it.contains(" ")) "\"${it.replace("\"", "\\\"")}\"" else it
        })
    }
}

fun main(args: Array<String>) {
    Timing.rootTimer.start()
    val runningTime = measureTime {
        FbSAT(args).main(args)
    }
    Timing.rootTimer.stop()
    Timing.rootTimer.pprint { logger.info(it) }
    logger.info("End time: ${DateTime.nowLocal().format(dateTimeFormat)}")
    logger.info("All done in %.3f seconds".format(runningTime.seconds))
}
