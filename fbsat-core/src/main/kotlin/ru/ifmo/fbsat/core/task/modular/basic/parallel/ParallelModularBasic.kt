package ru.ifmo.fbsat.core.task.modular.basic.parallel

import com.github.lipen.multiarray.forEachIndexed
import com.soywiz.klock.measureTimeWithResult
import okio.buffer
import okio.sink
import ru.ifmo.fbsat.core.automaton.ParallelModularAutomaton
import ru.ifmo.fbsat.core.automaton.buildBasicParallelModularAutomaton
import ru.ifmo.fbsat.core.scenario.inputEvent
import ru.ifmo.fbsat.core.scenario.inputValues
import ru.ifmo.fbsat.core.scenario.outputEvent
import ru.ifmo.fbsat.core.scenario.outputValues
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.convertBoolVarArray
import ru.ifmo.fbsat.core.solver.convertIntVarArray
import ru.ifmo.fbsat.core.task.Inferrer
import ru.ifmo.fbsat.core.task.optimizeParallelModularA
import ru.ifmo.fbsat.core.task.optimizeParallelModularT
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.ModularContext
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.ensureParentExists
import ru.ifmo.fbsat.core.utils.toBinaryString
import ru.ifmo.fbsat.core.utils.useWith
import ru.ifmo.fbsat.core.utils.write
import ru.ifmo.fbsat.core.utils.writeln

private val logger = MyLogger {}

fun Inferrer.parallelModularBasic(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    isEncodeReverseImplication: Boolean = true,
): ParallelModularAutomaton? {
    reset()
    declare(
        ParallelModularBasicTask(
            scenarioTree = scenarioTree,
            numberOfModules = numberOfModules,
            numberOfStates = numberOfStates,
            maxOutgoingTransitions = maxOutgoingTransitions,
            maxTransitions = maxTransitions,
            isEncodeReverseImplication = isEncodeReverseImplication
        )
    )
    return inferParallelModularBasic()
}

fun Inferrer.parallelModularBasicMinC(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    start: Int = 1, // C_start
    end: Int = 20, // C_end
): ParallelModularAutomaton {
    var best: ParallelModularAutomaton? = null
    for (C in start..end) {
        val (result, runningTime) = measureTimeWithResult {
            parallelModularBasic(
                scenarioTree = scenarioTree,
                numberOfModules = numberOfModules,
                numberOfStates = C
            )
        }
        if (result != null) {
            logger.info("ParallelModularBasicMin: C = $C -> SAT in %.3f s.".format(runningTime.seconds))
            logger.info("ParallelModularBasicMin: minimal C = $C")
            best = result
            break
        } else {
            logger.info("ParallelModularBasicMin: C = $C -> UNSAT in %.3f s.".format(runningTime.seconds))
        }
    }
    return checkNotNull(best) { "ParallelModularBasicMin: automaton not found." }
}

fun Inferrer.parallelModularBasicMin(
    scenarioTree: PositiveScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int? = null, // C_start, 1 if null
): ParallelModularAutomaton? {
    parallelModularBasicMinC(scenarioTree, numberOfModules = numberOfModules, start = numberOfStates ?: 1)
    return if (Globals.IS_ENCODE_CONJUNCTIVE_GUARDS) {
        optimizeParallelModularA()
    } else {
        optimizeParallelModularT()
    }
}

fun Inferrer.inferParallelModularBasic(): ParallelModularAutomaton? {
    val model = solveAndGetModel() ?: return null
    val automaton = buildBasicParallelModularAutomaton(solver.context, model)

    if (Globals.IS_ENCODE_CONJUNCTIVE_GUARDS) {
        val modularContext: ModularContext = solver.context["modularContext"]
        val moduleControllingOutputVariable =
            solver.context.convertIntVarArray("moduleControllingOutputVariable", model)
        modularContext.forEachIndexed { (m), ctx ->
            val scenarioTree: PositiveScenarioTree = ctx["scenarioTree"]
            val X: Int = ctx["X"]
            val Z: Int = ctx["Z"]
            val inputVariableUsed = ctx.convertBoolVarArray("inputVariableUsed", model)

            val sliceInput = (1..X).filter { x ->
                inputVariableUsed[x]
            }.map { it - 1 }
            val sliceOutput = (1..Z).filter { z ->
                moduleControllingOutputVariable[z] == m
            }.map { it - 1 }
            logger.debug("Slice input indices for m = $m: $sliceInput")
            logger.debug("Slice output indices for m = $m: $sliceOutput")

            val slicedScenarios = scenarioTree.scenarios.map {
                it.slice(sliceInput, sliceOutput)
            }
            val outFile = outDir.resolve("sliced-scenarios-m$m")
            logger.debug("Writing ${slicedScenarios.size} sliced scenario(s) for m = $m to '$outFile'")
            outFile.ensureParentExists().sink().buffer().useWith {
                writeln("${slicedScenarios.size}")
                for (scenario in slicedScenarios) {
                    for (element in scenario.elements) {
                        if (element.inputEvent != null) {
                            write("in=${element.inputEvent}[${element.inputValues.values.toBinaryString()}]; ")
                        }
                        if (element.outputEvent != null) {
                            write("out=${element.outputEvent}[${element.outputValues.values.toBinaryString()}]; ")
                        }
                    }
                    writeln()
                }
            }

            outDir.resolve("input-names-m$m").ensureParentExists().sink().buffer().useWith {
                for (name in scenarioTree.inputNames.slice(sliceInput)) {
                    writeln(name)
                }
            }
            outDir.resolve("output-names-m$m").ensureParentExists().sink().buffer().useWith {
                for (name in scenarioTree.outputNames.slice(sliceOutput)) {
                    writeln(name)
                }
            }
        }
    }

    // TODO: check automaton
    // log.warn("Mapping check is not implemented yet")

    return automaton
}
