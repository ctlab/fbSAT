package ru.ifmo.fbsat.core.utils.customlogging

import org.apache.logging.log4j.core.LogEvent
import org.apache.logging.log4j.core.config.plugins.Plugin
import org.apache.logging.log4j.core.pattern.ConverterKeys
import org.apache.logging.log4j.core.pattern.LogEventPatternConverter
import org.apache.logging.log4j.core.pattern.PatternConverter

@Plugin(name = "LoggerNameWithoutPrefixPatternConverter", category = PatternConverter.CATEGORY)
@ConverterKeys("loggernamewithoutprefix")
class LoggerNameWithoutPrefixPatternConverter private constructor(
    private val prefixes: Array<String>,
) : LogEventPatternConverter("LoggerNameWithoutPrefix", "loggernamewithoutprefix") {

    override fun format(event: LogEvent, toAppendTo: StringBuilder) {
        toAppendTo.append(convert(event.loggerName))
    }

    private fun convert(name: String): String {
        for (prefix in prefixes) {
            if (name.startsWith(prefix)) {
                return name.substring(prefix.length).removePrefix(".")
            }
        }
        return name
    }

    companion object {
        @JvmStatic
        fun newInstance(options: Array<String>): LoggerNameWithoutPrefixPatternConverter {
            return LoggerNameWithoutPrefixPatternConverter(options)
        }
    }
}
