package ru.ifmo.fbsat.core.task.distributed.basic

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import com.soywiz.klock.measureTimeWithResult
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.distributedBasicVars
import ru.ifmo.fbsat.core.task.optimizeDistributedSumC
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.multiArrayOfNulls

fun Inferrer.distributedBasic(
    numberOfModules: Int, // M
    compoundScenarioTree: PositiveCompoundScenarioTree, // TEMPORARILY
    modularScenarioTree: MultiArray<PositiveScenarioTree> = compoundScenarioTree.modular,
    modularNumberOfStates: MultiArray<Int>, // [C]
    modularMaxOutgoingTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [K]
    modularMaxTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [T]
    modularIsEncodeReverseImplication: MultiArray<Boolean> = MultiArray.create(numberOfModules) { true },
    maxTransitions: Int? = null // T_sum, unconstrained if null
): DistributedAutomaton? {
    reset()
    declare(
        DistributedBasicTask(
            numberOfModules = numberOfModules,
            compoundScenarioTree = compoundScenarioTree,
            modularScenarioTree = modularScenarioTree,
            modularNumberOfStates = modularNumberOfStates,
            modularMaxOutgoingTransitions = modularMaxOutgoingTransitions,
            modularMaxTransitions = modularMaxTransitions,
            modularIsEncodeReverseImplication = modularIsEncodeReverseImplication,
            maxTransitions = maxTransitions
        )
    )
    return inferDistributedBasic()
}

fun Inferrer.distributedBasicMinC(
    numberOfModules: Int, // M
    compoundScenarioTree: PositiveCompoundScenarioTree, // TEMPORARILY
    modularScenarioTree: MultiArray<PositiveScenarioTree> = compoundScenarioTree.modular,
    // modularStartNumberOfStates: MultiArray<Int> = MultiArray.create(numberOfModules){1}, // C_start
    // modularEndNumberOfStates: MultiArray<Int> = MultiArray.create(numberOfModules){20}, // C_end
    start: Int = 1, // C_start
    end: Int = 20, // C_end
    modularIsEncodeReverseImplication: MultiArray<Boolean> = MultiArray.create(numberOfModules) { true }
): DistributedAutomaton? {
    val M = numberOfModules
    var best: DistributedAutomaton? = null

    for (C in start..end) {
        val (result, runningTime) = measureTimeWithResult {
            distributedBasic(
                numberOfModules = M,
                compoundScenarioTree = compoundScenarioTree,
                modularScenarioTree = compoundScenarioTree.modular,
                modularNumberOfStates = MultiArray.create(M) { C },
                modularIsEncodeReverseImplication = modularIsEncodeReverseImplication
            )
        }
        if (result != null) {
            log.success("DistributedBasicMin: C = $C -> SAT in %.3f s.".format(runningTime.seconds))
            log.info(
                "DistributedBasicMin: minimal C = $C " +
                    "(${result.modular.map { it.numberOfStates }.values.joinToString("+")})"
            )
            best = result

            log.info("DistributedBasicMin: minimizing sum of C_i...")
            best = optimizeDistributedSumC()!!

            log.success(
                "DistributedBasicMin: minimal Cs = " +
                    "(${best.modular.map { it.numberOfStates }.values.joinToString("+")}) = " +
                    "${best.numberOfStates}"
            )

            break
        } else {
            log.failure("DistributedBasicMin: C = $C -> UNSAT in %.3f s.".format(runningTime.seconds))
        }
    }
    return best
}

fun Inferrer.inferDistributedBasic(): DistributedAutomaton? {
    val rawAssignment = solver.solve() ?: return null
    val vars = solver.context.distributedBasicVars
    val assignment = DistributedBasicAssignment.fromRaw(rawAssignment, vars)
    val automaton = assignment.toAutomaton()

    // TODO: check mapping
    // log.warn("Mapping check is not implemented yet")

    return automaton
}
