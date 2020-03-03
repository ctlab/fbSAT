package ru.ifmo.fbsat.core.task

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.utils.log

fun <T : Any> optimizeTopDown(
    start: Int? = null,
    end: Int = 0,
    nextInitial: (Int?) -> T?,
    next: (Int) -> T?,
    query: (T) -> Int
): T? {
    if (start != null) require(start >= end)
    if (start != null) require(start >= 0)
    require(end >= 0)

    val (initialResult, runningTime) = measureTimeWithResult { nextInitial(start) }
    if (initialResult != null) {
        log.success("optimizeTopDown: <= $start -> ${query(initialResult)} in %.2f s".format(runningTime.seconds))
    } else {
        log.failure("optimizeTopDown: <= $start -> UNSAT in %.2f s.".format(runningTime.seconds))
        return null
    }

    var best: T = initialResult
    while (true) {
        val x = query(best)
        if (x <= end) break
        val (result, runningTime) = measureTimeWithResult { next(x) }
        if (result != null) {
            log.success("optimizeTopDown: < $x -> ${query(result)} in %.2f s".format(runningTime.seconds))
            best = result
        } else {
            log.failure("optimizeTopDown: < $x -> UNSAT in %.2f s.".format(runningTime.seconds))
            break
        }
    }
    return best
}
