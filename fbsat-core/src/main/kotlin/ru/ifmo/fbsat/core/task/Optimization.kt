package ru.ifmo.fbsat.core.task

import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.ArbitraryModularAutomaton
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.automaton.ParallelModularAutomaton
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.task.distributed.basic.inferDistributedBasic
import ru.ifmo.fbsat.core.task.distributed.complete.inferDistributedComplete
import ru.ifmo.fbsat.core.task.distributed.extended.inferDistributedExtended
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.inferArbitraryModularBasic
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.inferConsecutiveModularBasic
import ru.ifmo.fbsat.core.task.modular.basic.parallel.inferParallelModularBasic
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.inferConsecutiveModularExtended
import ru.ifmo.fbsat.core.task.modular.extended.parallel.inferParallelModularExtended
import ru.ifmo.fbsat.core.task.single.basic.inferBasic
import ru.ifmo.fbsat.core.task.single.extended.inferExtended
import ru.ifmo.fbsat.core.task.single.extforest.inferExtForest
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
        log.success("optimizeTopDown: <= $start -> ${query(initialResult)} in %.3f s".format(runningTime.seconds))
    } else {
        log.failure("optimizeTopDown: <= $start -> UNSAT in %.3f s.".format(runningTime.seconds))
        return null
    }

    var best: T = initialResult
    while (true) {
        val x = query(best)
        if (x <= end) break
        val (result, runningTime) = measureTimeWithResult { next(x) }
        if (result != null) {
            log.success("optimizeTopDown: < $x -> ${query(result)} in %.3f s".format(runningTime.seconds))
            best = result
        } else {
            log.failure("optimizeTopDown: < $x -> UNSAT in %.3f s.".format(runningTime.seconds))
            break
        }
    }
    return best
}

fun Inferrer.optimizeT(start: Int? = null, end: Int = 0): Automaton? {
    log.info("Optimizing T...")
    val cardinalityT: Cardinality by solver.context
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { T ->
            cardinalityT.updateUpperBoundLessThanOrEqual(T)
            inferBasic()
        },
        next = { T ->
            cardinalityT.updateUpperBoundLessThan(T)
            inferBasic()
        },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.optimizeN(start: Int? = null, end: Int = 0): Automaton? {
    log.info("Optimizing N...")
    val cardinalityN: Cardinality by solver.context
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { N ->
            cardinalityN.updateUpperBoundLessThanOrEqual(N)
            inferExtended()
        },
        next = { N ->
            cardinalityN.updateUpperBoundLessThan(N)
            inferExtended()
        },
        query = { it.totalGuardsSize }
    )
}

@Suppress("FunctionName")
fun Inferrer.optimizeN_Forest(start: Int? = null, end: Int = 0): Automaton? {
    log.info("Optimizing N...")
    val cardinalityN: Cardinality by solver.context
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { N ->
            cardinalityN.updateUpperBoundLessThanOrEqual(N)
            inferExtForest()
        },
        next = { N ->
            cardinalityN.updateUpperBoundLessThan(N)
            inferExtForest()
        },
        query = { it.totalGuardsSize }
    )
}

fun Inferrer.optimizeParallelModularT(start: Int? = null, end: Int = 0): ParallelModularAutomaton? {
    log.info("Optimizing T...")
    val cardinalityT: Cardinality by solver.context
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { T ->
           cardinalityT.updateUpperBoundLessThanOrEqual(T)
            inferParallelModularBasic()
        },
        next = { T ->
           cardinalityT.updateUpperBoundLessThan(T)
            inferParallelModularBasic()
        },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.optimizeConsecutiveModularT(start: Int? = null, end: Int = 0): ConsecutiveModularAutomaton? {
    log.info("Optimizing T...")
    val cardinalityT: Cardinality by solver.context
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { T ->
            cardinalityT.updateUpperBoundLessThanOrEqual(T)
            inferConsecutiveModularBasic()
        },
        next = { T ->
            cardinalityT.updateUpperBoundLessThan(T)
            inferConsecutiveModularBasic()
        },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.optimizeArbitraryModularT(start: Int? = null, end: Int = 0): ArbitraryModularAutomaton? {
    log.info("Optimizing T...")
    val cardinalityT: Cardinality by solver.context
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { T ->
            cardinalityT.updateUpperBoundLessThanOrEqual(T)
            inferArbitraryModularBasic()
        },
        next = { T ->
            cardinalityT.updateUpperBoundLessThan(T)
            inferArbitraryModularBasic()
        },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.optimizeConsecutiveModularN(start: Int? = null, end: Int = 0): ConsecutiveModularAutomaton? {
    log.info("Optimizing N...")
    val cardinalityN: Cardinality by solver.context
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { N ->
           cardinalityN.updateUpperBoundLessThanOrEqual(N)
            inferConsecutiveModularExtended()
        },
        next = { N ->
           cardinalityN.updateUpperBoundLessThan(N)
            inferConsecutiveModularExtended()
        },
        query = { it.totalGuardsSize }
    )
}

fun Inferrer.optimizeParallelModularN(start: Int? = null, end: Int = 0): ParallelModularAutomaton? {
    log.info("Optimizing N...")
    val cardinalityN: Cardinality by solver.context
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { N ->
            cardinalityN.updateUpperBoundLessThanOrEqual(N)
            inferParallelModularExtended()
        },
        next = { N ->
            cardinalityN.updateUpperBoundLessThan(N)
            inferParallelModularExtended()
        },
        query = { it.totalGuardsSize }
    )
}

fun Inferrer.optimizeDistributedSumC(start: Int? = null, end: Int = 0): DistributedAutomaton? {
    log.info("Optimizing Csum...")
    val cardinalityC: Cardinality by solver.context
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { C ->
            cardinalityC.updateUpperBoundLessThanOrEqual(C)
            inferDistributedBasic()
        },
        next = { C ->
            cardinalityC.updateUpperBoundLessThan(C)
            inferDistributedBasic()
        },
        query = { it.numberOfReachableStates }
    )
}

fun Inferrer.optimizeDistributedSumC_Extended(start: Int? = null, end: Int = 0): DistributedAutomaton? {
    log.info("Optimizing Csum...")
    val cardinalityC: Cardinality by solver.context
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { C ->
            cardinalityC.updateUpperBoundLessThanOrEqual(C)
            inferDistributedExtended()
        },
        next = { C ->
            cardinalityC.updateUpperBoundLessThan(C)
            inferDistributedExtended()
        },
        query = { it.numberOfReachableStates }
    )
}

fun Inferrer.optimizeDistributedSumC_Complete(start: Int? = null, end: Int = 0): DistributedAutomaton? {
    log.info("Optimizing Csum...")
    val cardinalityC: Cardinality by solver.context
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { C ->
            cardinalityC.updateUpperBoundLessThanOrEqual(C)
            inferDistributedComplete()
        },
        next = { C ->
            cardinalityC.updateUpperBoundLessThan(C)
            inferDistributedComplete()
        },
        query = { it.numberOfReachableStates }
    )
}

fun Inferrer.optimizeDistributedSumN(start: Int? = null, end: Int = 0): DistributedAutomaton? {
    log.info("Optimizing Nsum...")
    val cardinalityN: Cardinality by solver.context
    return optimizeTopDown(
        start = start,
        end = end,
        nextInitial = { N ->
            cardinalityN.updateUpperBoundLessThanOrEqual(N)
            inferDistributedExtended()
        },
        next = { N ->
            cardinalityN.updateUpperBoundLessThan(N)
            inferDistributedExtended()
        },
        query = { it.totalGuardsSize }
    )
}
