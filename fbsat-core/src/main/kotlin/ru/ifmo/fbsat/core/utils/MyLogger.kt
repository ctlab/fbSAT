package ru.ifmo.fbsat.core.utils

import mu.KLogger
import mu.KMarkerFactory
import mu.KotlinLogging
import mu.Marker

val fbsatLogger: MyLogger = MyLogger("ru.ifmo.fbsat")

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
