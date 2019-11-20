package ru.ifmo.fbsat.core.task.single.complete

import com.soywiz.klock.DateTime
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.constraints.declareNegativeAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declareNegativeGuardConditionsConstraints
import ru.ifmo.fbsat.core.constraints.declareNegativeMappingConstraints
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.single.extended.ExtendedAssignment
import ru.ifmo.fbsat.core.task.single.extended.ExtendedTask
import ru.ifmo.fbsat.core.task.single.extended.toAutomaton
import ru.ifmo.fbsat.core.utils.TheAssignment
import ru.ifmo.fbsat.core.utils.checkMapping
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.secondsSince
import java.io.File

@Suppress("LocalVariableName")
class CompleteTask(
    scenarioTree: ScenarioTree,
    negativeScenarioTree: NegativeScenarioTree? = null, // empty if null
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxGuardSize: Int, // P
    maxTotalGuardsSize: Int? = null, // N, unconstrained if null
    val outDir: File,
    val solver: Solver,
    val autoFinalize: Boolean = true
) {
    val vars: CompleteVariables

    private val extendedTask = ExtendedTask(
        scenarioTree = scenarioTree,
        numberOfStates = numberOfStates,
        maxOutgoingTransitions = maxOutgoingTransitions,
        maxGuardSize = maxGuardSize,
        maxTotalGuardsSize = null,
        outDir = outDir,
        solver = solver,
        autoFinalize = false,
        isEncodeReverseImplication = false
    )

    init {
        val timeStart = DateTime.nowLocal()
        val nvarStart = solver.numberOfVariables
        val nconStart = solver.numberOfClauses

        vars = CompleteVariables(
            extVars = extendedTask.vars,
            negativeScenarioTree = negativeScenarioTree ?: NegativeScenarioTree(
                inputEvents = scenarioTree.inputEvents,
                outputEvents = scenarioTree.outputEvents,
                inputNames = scenarioTree.inputNames,
                outputNames = scenarioTree.outputNames
            )
        )

        updateCardinality(maxTotalGuardsSize)
        updateNegativeReduction()

        val nvarDiff = solver.numberOfVariables - nvarStart
        val nconDiff = solver.numberOfClauses - nconStart
        log.info(
            "CompleteTask: Done declaring variables ($nvarDiff) and constraints ($nconDiff) in %.2f s"
                .format(secondsSince(timeStart))
        )
    }

    fun addNegativeScenario(negativeScenario: NegativeScenario) {
        vars.negativeScenarioTree.addNegativeScenario(negativeScenario)
        updateNegativeReduction()
    }

    fun addNegativeScenarios(negativeScenarios: Iterable<NegativeScenario>) {
        for (scenario in negativeScenarios) {
            vars.negativeScenarioTree.addNegativeScenario(scenario)
        }
        updateNegativeReduction()
    }

    fun updateNegativeReduction() {
        with(solver) {
            with(vars) {
                // TODO: timeit

                /* Constants */
                val oldNegV = negV
                negV = negativeScenarioTree.size
                val newNegVs = (oldNegV + 1)..negV

                negU = negativeScenarioTree.uniqueInputs.size

                val oldNegUIs = negUIs
                negUIs = negativeScenarioTree.uniqueInputs

                fun getOldNegU(input: InputValues): Int = oldNegUIs.indexOf(input) + 1
                fun getNegU(input: InputValues): Int = negUIs.indexOf(input) + 1
                fun getPosU(input: InputValues): Int = posUIs.indexOf(input) + 1
                fun inputChoice(u: Int, inOldNegUIs: (Int) -> Int, inPosUIs: (Int) -> Int): Int =
                    when (val input = negUIs[u - 1]) {
                        in oldNegUIs -> inOldNegUIs(getOldNegU(input))
                        in posUIs -> inPosUIs(getPosU(input))
                        else -> newVariable()
                    }

                val oldOnlyNegUIs = onlyNegUIs
                onlyNegUIs = negUIs - posUIs
                val newOnlyNegUIs = onlyNegUIs - oldOnlyNegUIs
                val newOnlyNegUs = newOnlyNegUIs.map(::getNegU)

                /* Variables */
                negTransitionFiring = newArray(C, K, negU) { (c, k, u) ->
                    inputChoice(u,
                        { negTransitionFiring[c, k, it] },
                        { transitionFiring[c, k, it] })
                }
                negFirstFired = newArray(C, negU, K + 1, one = true) { (c, u, k) ->
                    inputChoice(u,
                        { negFirstFired[c, it, k] },
                        { firstFired[c, it, k] })
                }
                negNotFired = newArray(C, K, negU) { (c, k, u) ->
                    inputChoice(u,
                        { negNotFired[c, k, it] },
                        { notFired[c, k, it] })
                }
                negActualTransitionFunction = newArray(C, E, negU, C + 1, one = true) { (c, e, u, c2) ->
                    inputChoice(u,
                        { negActualTransitionFunction[c, e, it, c2] },
                        { actualTransitionFunction[c, e, it, c2] })
                }
                negNodeValue = newArray(C, K, P, negU) { (c, k, p, u) ->
                    if (p == 1) negTransitionFiring[c, k, u]
                    else inputChoice(u,
                        { negNodeValue[c, k, p, it] },
                        { nodeValue[c, k, p, it] })
                }
                negMapping = newArray(negV, C + 1, one = true) { (v, c) ->
                    if (v in newNegVs) newVariable()
                    else negMapping[v, c]
                }

                /* Constraints */
                declareNegativeAutomatonStructureConstraints(vars, Us = newOnlyNegUs)
                declareNegativeGuardConditionsConstraints(vars, Us = newOnlyNegUs)
                declareNegativeMappingConstraints(vars, Vs = newNegVs)
            }
        }
    }

    fun updateCardinality(newMaxTotalGuardsSize: Int?) {
        extendedTask.updateCardinality(newMaxTotalGuardsSize)
        vars.totalizer = extendedTask.vars.totalizer
        vars.maxTotalGuardsSize = extendedTask.vars.maxTotalGuardsSize
    }

    fun infer(): Automaton? {
        val rawAssignment = solver.solve()?.data
        if (autoFinalize) finalize2()
        if (rawAssignment == null) return null

        val assignment = ExtendedAssignment.fromRaw(rawAssignment, extendedTask.vars)
        val automaton = assignment.toAutomaton()

        with(vars) {
            check(
                checkMapping(
                    automaton = automaton,
                    scenarios = scenarioTree.scenarios,
                    mapping = assignment.mapping
                )
            ) { "Positive mapping mismatch" }
            check(
                checkMapping(automaton = automaton,
                    scenarios = negativeScenarioTree.negativeScenarios,
                    mapping = with(object : TheAssignment {}) {
                        rawAssignment.convertIntArray(negMapping, negV, domain = 1..C) { 0 }
                    }
                )
            ) { "Negative mapping mismatch" }
        }

        return automaton
    }

    fun finalize2() {
        extendedTask.finalize2()
        // Note: extendedTask already finalizes the solver, so it is not necessary to do it here again
        // solver.finalize2()
    }
}
