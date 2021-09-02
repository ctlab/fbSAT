package ru.ifmo.fbsat.core.utils

import com.soywiz.klock.PerformanceCounter
import com.soywiz.klock.TimeSpan
import kotlin.math.roundToInt

private val logger = MyLogger {}

object Timing {
    val rootTimer: Timer = Timer("/")

    private val stack: ArrayDeque<Timer> = ArrayDeque()
    private val currentTimer: Timer
        get() = stack.first()

    init {
        stack.addFirst(rootTimer)
    }

    fun push(name: String) {
        val timer = currentTimer.subtimer(name).also { it.start() }
        stack.addFirst(timer)
    }

    fun pop(): TimeSpan {
        val timer = stack.removeFirst()
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
) {
    private val _subtimers: MutableMap<String, Timer> = mutableMapOf()
    val subtimers: Map<String, Timer> = _subtimers

    private val _deltas: MutableList<TimeSpan> = mutableListOf()
    val deltas: List<TimeSpan> = _deltas

    val totalTime: Double
        get() = deltas.sumOf { it.seconds }

    private var startTime: TimeSpan = TimeSpan.NIL

    var isStarted: Boolean = false
        private set
    var isStopped: Boolean = false
        private set

    fun subtimer(name: String): Timer {
        return _subtimers.computeIfAbsent(name) { Timer(it) }
    }

    fun start() {
        // logger.debug { "Starting timer '$name'" }

        if (isStarted) {
            error("Timer '$name' has already been started")
        }

        startTime = PerformanceCounter.reference
        isStarted = true
        isStopped = false
    }

    fun stop(): TimeSpan {
        // logger.debug { "Stopping timer '$name'" }

        if (!isStarted) {
            error("Timer '$name' has not been started")
        }
        if (isStopped) {
            error("Timer '$name' has already been stopped")
        }

        for (timer in subtimers.values) {
            if (!timer.isStopped) {
                logger.warn("Stopping a running subtimer '${timer.name}'")
                timer.stop()
            }
        }

        val delta = PerformanceCounter.reference - startTime
        _deltas.add(delta)

        // logger.debug { "Timer '$name' delta: ${"%.3fs".format(delta.seconds)}" }

        startTime = TimeSpan.NIL
        isStarted = false
        isStopped = true

        return delta
    }

    fun toLines(
        externalTotalTime: Double = 0.0,
    ): Sequence<String> = sequence {
        require(externalTotalTime >= 0)

        // cache the value
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
            val minutes = (s % 3600) / 60
            val seconds = s % 60
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

        yield("$timeString [$percentString]  $name")
        // yield("[$percentString] $timeString  $name")

        // F - first, S - sub
        val prefixFF = "├─╴"
        val prefixFS = "│  "
        val prefixSF = "└─╴"
        val prefixSS = "   "

        if (subtimers.isNotEmpty()) {
            val lineGroups: MutableList<Sequence<String>> = mutableListOf()

            for (timer in subtimers.values) {
                val lines = timer.toLines(totalTime)
                lineGroups.add(lines)
            }

            // TODO: add 'other' if necessary

            // yield lines from all groups except the last one
            for (group in lineGroups.dropLast(1)) {
                val iter = group.iterator()
                check(iter.hasNext())
                yield("$prefixFF${iter.next()}")
                for (line in iter) {
                    yield("$prefixFS$line")
                }
            }

            // yield lines from the last group
            if (lineGroups.isNotEmpty()) {
                val lastGroup = lineGroups.last()
                val iter = lastGroup.iterator()
                check(iter.hasNext())
                yield("$prefixSF${iter.next()}")
                for (line in iter) {
                    yield("$prefixSS$line")
                }
            }
        }
    }

    fun pprint(p: (String) -> Unit) {
        toLines().forEach(p)
    }
}
