package ru.ifmo.fbsat.core.task.distributed.complete

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.automaton.buildExtendedDistributedAutomaton
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.distributed.basic.DistributedBasicTask
import ru.ifmo.fbsat.core.task.distributed.extended.DistributedExtendedTask
import ru.ifmo.fbsat.core.task.distributed.extended.inferDistributedExtended
import ru.ifmo.fbsat.core.task.optimizeDistributedSumC_Complete
import ru.ifmo.fbsat.core.task.optimizeDistributedSumN
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.multiArrayOfNulls
import kotlin.math.min

private val logger = MyLogger {}

fun Inferrer.distributedComplete(
    numberOfModules: Int, // M
    compoundScenarioTree: PositiveCompoundScenarioTree, // TEMPORARILY
    modularScenarioTree: MultiArray<PositiveScenarioTree>,
    negativeCompoundScenarioTree: NegativeCompoundScenarioTree? = null,
    modularNumberOfStates: MultiArray<Int>, // [C]
    modularMaxOutgoingTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [K]
    modularMaxGuardSize: MultiArray<Int>, // [P]
    modularMaxTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [T]
    modularMaxTotalGuardsSize: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [N]
    modularIsEncodeReverseImplication: MultiArray<Boolean> = MultiArray.new(numberOfModules) { true },
    maxTransitions: Int? = null, // T_sum, unconstrained if null
    maxTotalGuardsSize: Int? = null, // N_sum, unconstrained if null
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
    declare(
        DistributedExtendedTask(
            numberOfModules = numberOfModules,
            modularMaxGuardSize = modularMaxGuardSize,
            modularMaxTotalGuardsSize = modularMaxTotalGuardsSize,
            maxTotalGuardsSize = maxTotalGuardsSize
        )
    )
    declare(
        DistributedCompleteTask(
            numberOfModules = numberOfModules,
            negativeCompoundScenarioTree = negativeCompoundScenarioTree
        )
    )
    return inferDistributedExtended()
}

fun Inferrer.completeMin_(
    numberOfModules: Int, // M
    compoundScenarioTree: PositiveCompoundScenarioTree, // TEMPORARILY
    modularScenarioTree: MultiArray<PositiveScenarioTree>,
    negativeCompoundScenarioTree: NegativeCompoundScenarioTree? = null,
    modularNumberOfStates: MultiArray<Int>, // [C]
    modularMaxOutgoingTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [K]
    modularMaxGuardSize: MultiArray<Int>, // [P]
    modularMaxTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [T]
    modularIsEncodeReverseImplication: MultiArray<Boolean> = MultiArray.new(numberOfModules) { true },
    maxTransitions: Int? = null, // T_sum, unconstrained if null
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
    declare(
        DistributedExtendedTask(
            numberOfModules = numberOfModules,
            modularMaxGuardSize = modularMaxGuardSize
        )
    )
    declare(
        DistributedCompleteTask(
            numberOfModules = numberOfModules,
            negativeCompoundScenarioTree = negativeCompoundScenarioTree
        )
    )
    return optimizeDistributedSumN()
}

fun Inferrer.completeMin__(
    numberOfModules: Int, // M
    compoundScenarioTree: PositiveCompoundScenarioTree, // TEMPORARILY
    modularScenarioTree: MultiArray<PositiveScenarioTree>,
    negativeCompoundScenarioTree: NegativeCompoundScenarioTree? = null,
    // modularNumberOfStates: MultiArray<Int>, // [C]
    // modularMaxOutgoingTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [K]
    modularMaxGuardSize: MultiArray<Int>, // [P]
    modularMaxTransitions: MultiArray<Int?> = multiArrayOfNulls(numberOfModules), // [T]
    modularIsEncodeReverseImplication: MultiArray<Boolean> = MultiArray.new(numberOfModules) { true },
    maxTransitions: Int? = null, // T_sum, unconstrained if null
    startD: Int = 1,
): DistributedAutomaton? {
    // for (D in startD..100) {
    for (D in startD..5) {
        logger.info("Trying D = $D...")
        // val automaton = completeMin_(
        //     numberOfModules = numberOfModules,
        //     compoundScenarioTree = compoundScenarioTree,
        //     modularScenarioTree = modularScenarioTree,
        //     negativeCompoundScenarioTree = negativeCompoundScenarioTree,
        //     modularNumberOfStates = MultiArray.new(numberOfModules) { D },
        //     modularMaxGuardSize = modularMaxGuardSize,
        //     modularMaxTransitions = modularMaxTransitions,
        //     modularIsEncodeReverseImplication = modularIsEncodeReverseImplication,
        //     maxTransitions = maxTransitions
        // )
        val automaton = distributedComplete(
            numberOfModules = numberOfModules,
            compoundScenarioTree = compoundScenarioTree,
            modularScenarioTree = modularScenarioTree,
            negativeCompoundScenarioTree = negativeCompoundScenarioTree,
            modularNumberOfStates = MultiArray.new(numberOfModules) { D },
            modularMaxGuardSize = modularMaxGuardSize,
            modularMaxTransitions = modularMaxTransitions,
            modularIsEncodeReverseImplication = modularIsEncodeReverseImplication,
            maxTransitions = maxTransitions
        )
        if (automaton != null) {
            val automatonMinSumC = optimizeDistributedSumC_Complete()!!
            // for (m in 1..numberOfModules) {
            //     log.info("automatonMinSumC (module $m):")
            //     automatonMinSumC.project(m).pprint()
            // }
            // log.info("modularNumberOfReachableStates = ${automatonMinSumC.modules.map { it.numberOfReachableStates }.values}")
            // log.info("modularNumberOfStates = ${automatonMinSumC.modules.map { it.numberOfStates }.values}")
            val reinferred = distributedComplete(
                numberOfModules = numberOfModules,
                compoundScenarioTree = compoundScenarioTree,
                modularScenarioTree = modularScenarioTree,
                negativeCompoundScenarioTree = negativeCompoundScenarioTree,
                modularNumberOfStates = automatonMinSumC.modules.map { it.numberOfReachableStates },
                modularMaxOutgoingTransitions = MultiArray.new(numberOfModules) { D },
                modularMaxGuardSize = modularMaxGuardSize,
                modularMaxTransitions = modularMaxTransitions,
                modularIsEncodeReverseImplication = modularIsEncodeReverseImplication,
                maxTransitions = maxTransitions
            )!!
            return optimizeDistributedSumN(
                start = min(
                    automatonMinSumC.totalGuardsSize,
                    reinferred.totalGuardsSize
                )
            )!!
        }
    }
    return null
}

fun Inferrer.inferDistributedComplete(): DistributedAutomaton? {
    val model = solveAndGetModel() ?: return null
    // val vars = solver.context.distributedExtendedVars
    // val assignment = DistributedExtendedAssignment.frommodel(model, vars)
    // val automaton = assignment.toAutomaton()
    val automaton = buildExtendedDistributedAutomaton(
        context = solver.context,
        model = model
    )

    // TODO: check mapping
    // log.warn("Mapping check is not implemented yet")

    // val completeVars = solver.context.distributedCompleteVars
    // val modularNegMapping = completeVars.modularCompleteVariables.map {
    //     it.negMapping.convert(model)
    // }
    // println("Negative mapping:")
    // for (v in 1..completeVars.negativeCompoundScenarioTree.size) {
    //     println(
    //         "v = ${v.toString().padStart(2)} -> ${modularNegMapping.map { it[v] }.values}" +
    //             (if (completeVars.negativeCompoundScenarioTree.nodes[v - 1].isLoopBack) " [loop-back]" else "") +
    //             (if (completeVars.negativeCompoundScenarioTree.nodes[v - 1].children.isEmpty()) " [leaf]" else "") +
    //             (if (completeVars.negativeCompoundScenarioTree.nodes[v - 1].loopBacks.isNotEmpty()) " [loop-backs: ${completeVars.negativeCompoundScenarioTree.nodes[v - 1].loopBacks.map { it.id }}]" else "") +
    //             " ${completeVars.negativeCompoundScenarioTree.nodes[v - 1]}"
    //     )
    // }

    // val completeVars = solver.context.distributedCompleteVars.modularCompleteVariables[1]
    // val negTree = completeVars.negativeScenarioTree
    // val negMapping = completeVars.negMapping.convert(model)

    // for (scenario in negTree.scenarios) {
    //     val automatonNegMapping = automaton.modular[1].map(scenario)
    //     for ((element, state) in scenario.elements.zip(automatonNegMapping)) {
    //         val v = element.nodeId!!
    //         println("automatonNegMapping[v = $v] = ${state?.id}, negMapping[v = $v] = ${negMapping[v]}")
    //         if (state != null && state.id != negMapping[v]) {
    //             log.error("mapping mismatch")
    //         }
    //     }
    // }

    // if (automaton.verify(solver.context.distributedCompleteVars.negativeCompoundScenarioTree)) {
    //     log.success("Post-infer negative compound scenario tree verify: OK")
    // } else {
    //     log.failure("Post-infer negative compound scenario tree verify: FAILURE")
    //     error("Sad")
    // }

    return automaton
}
