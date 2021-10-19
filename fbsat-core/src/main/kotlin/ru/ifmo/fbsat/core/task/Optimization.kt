package ru.ifmo.fbsat.core.task

import com.github.lipen.satlib.card.Cardinality
import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.ArbitraryModularAutomaton
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.automaton.ParallelModularAutomaton
import ru.ifmo.fbsat.core.automaton.getA
import ru.ifmo.fbsat.core.automaton.getTA
import ru.ifmo.fbsat.core.task.distributed.basic.inferDistributedBasic
import ru.ifmo.fbsat.core.task.distributed.complete.inferDistributedComplete
import ru.ifmo.fbsat.core.task.distributed.extended.inferDistributedExtended
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.inferArbitraryModularBasic
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.inferConsecutiveModularBasic
import ru.ifmo.fbsat.core.task.modular.basic.parallel.inferParallelModularBasic
import ru.ifmo.fbsat.core.task.modular.extended.arbitrary.inferArbitraryModularExtended
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.inferConsecutiveModularExtended
import ru.ifmo.fbsat.core.task.modular.extended.parallel.inferParallelModularExtended
import ru.ifmo.fbsat.core.task.single.basic.inferBasic
import ru.ifmo.fbsat.core.task.single.extended.inferExtended
import ru.ifmo.fbsat.core.task.single.extforest.inferExtForest
import ru.ifmo.fbsat.core.task.single.extprecomp.inferExtPreComp
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.MyLogger

private val logger = MyLogger {}

fun <T : Any> optimizeTopDown(
    start: Int? = null,
    end: Int = 0,
    nextInitial: (Int?) -> T?,
    next: (Int) -> T?,
    query: (T) -> Int,
): T? {
    if (start != null) require(start >= end)
    if (start != null) require(start >= 0)
    require(end >= 0)

    val (initialResult, runningTime) = measureTimeWithResult { nextInitial(start) }
    if (initialResult != null) {
        logger.info("optimizeTopDown: <= $start -> ${query(initialResult)} in %.3f s".format(runningTime.seconds))
    } else {
        logger.info("optimizeTopDown: <= $start -> UNSAT in %.3f s.".format(runningTime.seconds))
        return null
    }

    var best: T = initialResult
    while (true) {
        val x = query(best)
        if (x <= end) break
        val (result, runningTime) = measureTimeWithResult { next(x) }
        if (result != null) {
            logger.info("optimizeTopDown: < $x -> ${query(result)} in %.3f s".format(runningTime.seconds))
            best = result
        } else {
            logger.info("optimizeTopDown: < $x -> UNSAT in %.3f s.".format(runningTime.seconds))
            break
        }
    }
    return best
}

fun <T : Any> Cardinality.optimizeTopDown(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
    infer: () -> T?,
    query: (T) -> Int,
): T? = optimizeTopDown(
    start = start,
    end = end,
    nextInitial = { bound ->
        if (useAssumptions) assumeUpperBoundLessThanOrEqual(bound)
        else declareUpperBoundLessThanOrEqual(bound)
        infer()
    },
    next = { bound ->
        if (useAssumptions) assumeUpperBoundLessThan(bound)
        else declareUpperBoundLessThan(bound)
        infer()
    },
    query = query
)

fun Inferrer.optimizeT(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): Automaton? {
    logger.info("Optimizing T (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityT"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferBasic() },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.optimizeN(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): Automaton? {
    logger.info("Optimizing N (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityN"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferExtended() },
        query = { it.totalGuardsSize }
    )
}

@Suppress("FunctionName")
fun Inferrer.optimizeTA(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): Automaton? {
    logger.info("Optimizing T*A (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityTA"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferBasic() },
        query = { it.getTA()!! }
    )
}

@Suppress("FunctionName")
fun Inferrer.optimizeN_Forest(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): Automaton? {
    logger.info("Optimizing N (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityN"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferExtForest() },
        query = { it.totalGuardsSize }
    )
}

@Suppress("FunctionName")
fun Inferrer.optimizeN_PreComp(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): Automaton? {
    logger.info("Optimizing N (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityN"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferExtPreComp() },
        query = { it.totalGuardsSize }
    )
}

fun Inferrer.optimizeParallelModularT(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): ParallelModularAutomaton? {
    logger.info("Optimizing T (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityT"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferParallelModularBasic() },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.optimizeConsecutiveModularT(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): ConsecutiveModularAutomaton? {
    logger.info("Optimizing T (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityT"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferConsecutiveModularBasic() },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.optimizeArbitraryModularT(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): ArbitraryModularAutomaton? {
    logger.info("Optimizing T (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityT"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferArbitraryModularBasic() },
        query = { it.numberOfTransitions }
    )
}

fun Inferrer.optimizeParallelModularN(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): ParallelModularAutomaton? {
    logger.info("Optimizing N (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityN"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferParallelModularExtended() },
        query = { it.totalGuardsSize }
    )
}

fun Inferrer.optimizeConsecutiveModularN(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): ConsecutiveModularAutomaton? {
    logger.info("Optimizing N (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityN"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferConsecutiveModularExtended() },
        query = { it.totalGuardsSize }
    )
}

fun Inferrer.optimizeArbitraryModularN(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): ArbitraryModularAutomaton? {
    logger.info("Optimizing N (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityN"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferArbitraryModularExtended() },
        query = { it.totalGuardsSize }
    )
}

fun Inferrer.optimizeParallelModularA(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): ParallelModularAutomaton? {
    logger.info("Optimizing A (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityA"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferParallelModularBasic() },
        query = { it.getA()!! }
    )
}

fun Inferrer.optimizeParallelModularCKA(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): ParallelModularAutomaton? {
    logger.info("Optimizing C*K*A (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityCKA"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferParallelModularBasic() },
        query = {
            it.modules.values.sumOf { module ->
                module.numberOfTransitions * module.maxOutgoingTransitions * module.getA()!!
            }
        }
    )
}

fun Inferrer.optimizeDistributedSumC(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): DistributedAutomaton? {
    logger.info("Optimizing Csum (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityC"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferDistributedBasic() },
        query = { it.numberOfReachableStates }
    )
}

fun Inferrer.optimizeDistributedSumC_Extended(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): DistributedAutomaton? {
    logger.info("Optimizing Csum (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityC"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferDistributedExtended() },
        query = { it.numberOfReachableStates }
    )
}

fun Inferrer.optimizeDistributedSumC_Complete(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): DistributedAutomaton? {
    logger.info("Optimizing Csum (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityC"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferDistributedComplete() },
        query = { it.numberOfReachableStates }
    )
}

fun Inferrer.optimizeDistributedSumN(
    start: Int? = null,
    end: Int = 0,
    useAssumptions: Boolean = Globals.IS_USE_ASSUMPTIONS,
): DistributedAutomaton? {
    logger.info("Optimizing Nsum (${if (useAssumptions) "with" else "without"} assumptions) from $start to $end...")
    val cardinality: Cardinality = solver.context["cardinalityN"]
    return cardinality.optimizeTopDown(
        start = start,
        end = end,
        useAssumptions = useAssumptions,
        infer = { inferDistributedExtended() },
        query = { it.totalGuardsSize }
    )
}
