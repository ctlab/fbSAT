package ru.ifmo.fbsat.core.utils

import mu.KLogger
import mu.KMarkerFactory
import mu.KotlinLogging
import mu.Marker
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.core.LoggerContext
import org.apache.logging.log4j.core.appender.FileAppender
import org.apache.logging.log4j.core.config.Configuration
import org.apache.logging.log4j.core.layout.PatternLayout

const val FBSAT_LOGGER_NAME: String = "ru.ifmo.fbsat"

val fbsatLogger: MyLogger = MyLogger(FBSAT_LOGGER_NAME)

class MyLogger(val delegate: KLogger) {

    constructor(func: () -> Unit) : this(KotlinLogging.logger(func))

    constructor(name: String) : this(KotlinLogging.logger(name))

    fun debug(msg: String) {
        if (Globals.IS_DEBUG) {
            delegate.debug(msg)
        } else {
            delegate.debug(NO_CONSOLE, msg)
        }
    }

    fun info(msg: String) {
        delegate.info(msg)
    }

    fun warn(msg: String) {
        delegate.warn(msg)
    }

    fun error(msg: String) {
        delegate.error(msg)
    }

    inline fun debug(msg: () -> String) {
        debug(msg())
    }

    inline fun info(msg: () -> String) {
        info(msg())
    }

    inline fun warn(msg: () -> String) {
        warn(msg())
    }

    inline fun error(msg: () -> String) {
        error(msg())
    }

    fun just(msg: String) {
        println(msg)
        delegate.info(NO_CONSOLE, msg)
    }

    companion object {
        /**
         * Messages to STDOUT with [NO_CONSOLE] marker are filtered out via MarkerFilter.
         */
        val NO_CONSOLE: Marker = KMarkerFactory.getMarker("NO_CONSOLE")
    }
}

private class FileBuilder : FileAppender.Builder<FileBuilder>()

private fun Configuration.createFileAppender(build: FileAppender.Builder<FileBuilder>.() -> Unit): FileAppender =
    (FileAppender.newBuilder<FileBuilder>() as FileAppender.Builder<FileBuilder>)
        .apply(build)
        .apply {
            setConfiguration(this@createFileAppender)
        }
        .build()

fun setupLogFileAppender(
    path: String = "logs/fbsat.log",
    name: String = "logFile",
    append: Boolean = false,
    pattern: String = "[%date{yyyy-MM-dd HH:mm:ss}] [%level] %logger: %message%n%throwable",
) {
    val ctx = LogManager.getContext(false) as LoggerContext
    ctx.configuration.run {
        val logFile = createFileAppender {
            setName(name)
            withAppend(append)
            withFileName(path)
            setLayout(PatternLayout.newBuilder()
                .withPattern(pattern)
                .build())
        }
        logFile.start()
        rootLogger.addAppender(logFile, Level.DEBUG, null)
        loggers[FBSAT_LOGGER_NAME]?.addAppender(logFile, Level.DEBUG, null)
    }
    ctx.updateLoggers()
}
