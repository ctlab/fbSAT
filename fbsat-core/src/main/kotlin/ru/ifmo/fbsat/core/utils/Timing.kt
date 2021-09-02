package ru.ifmo.fbsat.core.utils

import com.soywiz.klock.PerformanceCounter
import com.soywiz.klock.TimeSpan
import kotlin.math.roundToInt

private val logger = MyLogger {}

object Timing {
    val rootTimer: Timer = Timer("fbsat", null)

    var currentTimer: Timer = rootTimer
        private set

    fun push(name: String) {
        val timer = currentTimer.subtimer(name)
        timer.start()
        currentTimer = timer
    }

    fun pop(): TimeSpan {
        val timer = currentTimer
        currentTimer = timer.parent
            ?: error("Cannot pop a timer without a parent")
        return timer.stop()
    }
}

inline fun <T> withTimer(name: String, block: () -> T): T {
    Timing.push(name)
    try {
        return block()
    } finally {
        Timing.pop()
    }
}

class Timer(
    val name: String,
    val parent: Timer? = Timing.rootTimer,
) {
    private val _subtimers: MutableMap<String, Timer> = mutableMapOf()
    val subtimers: Map<String, Timer> = _subtimers

    private val _deltas: MutableList<TimeSpan> = mutableListOf()
    val deltas: List<TimeSpan> = _deltas

    private var startTime: TimeSpan = TimeSpan.NIL

    var isStarted: Boolean = false
        private set
    var isStopped: Boolean = false
        private set

    val totalTime: Double
        get() = deltas.sumOf { it.seconds }

    val fullName: String =
        if (parent == null) {
            name
        } else {
            parent.fullName + "/" + name
        }

    fun subtimer(name: String): Timer {
        return _subtimers.computeIfAbsent(name) {
            Timer(it, this)
        }
    }

    fun start() {
        // logger.debug { "Starting timer '$fullName'" }

        if (isStarted) {
            error("Timer '$name' has already been started")
        }

        startTime = PerformanceCounter.reference
        isStarted = true
        isStopped = false
    }

    fun stop(): TimeSpan {
        // logger.debug { "Stopping timer '$fullName'" }

        if (!isStarted) {
            error("Timer '$name' has not been started")
        }
        if (isStopped) {
            error("Timer '$name' has already been stopped")
        }

        for (timer in subtimers.values) {
            if (!timer.isStopped) {
                logger.warn("Stopping a running subtimer '${timer.fullName}'")
                timer.stop()
            }
        }

        val delta = PerformanceCounter.reference - startTime
        _deltas.add(delta)

        // logger.debug { "Timer '$fullName' delta: ${"%.3fs".format(delta.seconds)}" }

        startTime = TimeSpan.NIL
        isStarted = false
        isStopped = true

        return delta
    }

    fun pprint(
        externalTotalTime: Double = 0.0,
        prefixFirst: String = "",
        prefixSub: String = "",
        p: (String) -> Unit,
    ) {
        require(externalTotalTime >= 0)
        val totalTime = totalTime
        val percent = if (externalTotalTime > 0) {
            totalTime / externalTotalTime * 100.0
        } else {
            100.0
        }
        val percentString = "%.0f".format(percent) + "%"
        val timeSecondsString = when {
            totalTime < .9995 -> "%.3f".format(totalTime)
            totalTime < 9.995 -> "%.2f".format(totalTime)
            totalTime < 99.95 -> "%.1f".format(totalTime)
            else -> "%.0f".format(totalTime)
        } + "s"
        val timeHumanString = if (totalTime >= 60) {
            val s = totalTime.roundToInt()
            val hours = s / 3600
            val minutes = (s - hours * 3600) / 60
            val seconds = s - hours * 3600 - minutes * 60
            check(hours * 3600 + minutes * 60 + seconds == s)
            "${
                if (hours > 0) "${hours}h " else ""
            }${
                if (minutes > 0) "${minutes}m " else ""
            }${
                if (seconds > 0) "${seconds}s " else ""
            }".trim()
        } else {
            null
        }
        val timeString = timeSecondsString +
            (if (timeHumanString != null) " ($timeHumanString)" else "")

        // p("$prefixFirst${"%2.0f".format(percent)}% ${"%6.2f".format(totalTime)}s  $name")
        // p("$prefixFirst$timeString [$percentString]  $name")
        p("$prefixFirst[$percentString] $timeString  $name")

        for ((i, timer) in subtimers.values.withIndex()) {
            if (i < subtimers.size - 1) {
                timer.pprint(totalTime, "├─╴", "│ ") { p("$prefixSub  $it") }
            } else {
                timer.pprint(totalTime, "└─╴", "  ") { p("$prefixSub  $it") }
            }
        }
    }
}
