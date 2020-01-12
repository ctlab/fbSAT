package ru.ifmo.fbsat.core.task.single.complete

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.constraints.declareNegativeAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declareNegativeGuardConditionsConstraints
import ru.ifmo.fbsat.core.constraints.declareNegativeMappingConstraints
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.IntVar
import ru.ifmo.fbsat.core.solver.IntVarArray
import ru.ifmo.fbsat.core.solver.Literal
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.convert
import ru.ifmo.fbsat.core.task.single.extended.ExtendedAssignment
import ru.ifmo.fbsat.core.task.single.extended.ExtendedTask
import ru.ifmo.fbsat.core.task.single.extended.toAutomaton
import ru.ifmo.fbsat.core.utils.checkMapping
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.measureTimeOnce
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
    internal val cardinality: Cardinality

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
        val nvarStart = solver.numberOfVariables
        val nconStart = solver.numberOfClauses
        val timeDeclare = measureTimeOnce {

            with(solver) {
                /* Variables */
                vars = declareCompleteVariables(
                    extendedVars = extendedTask.vars,
                    negativeScenarioTree = negativeScenarioTree ?: NegativeScenarioTree(
                        inputEvents = scenarioTree.inputEvents,
                        outputEvents = scenarioTree.outputEvents,
                        inputNames = scenarioTree.inputNames,
                        outputNames = scenarioTree.outputNames
                    )
                )

                /* Cardinality */
                cardinality = extendedTask.cardinality
            }

            /* Initial cardinality constraints */
            updateCardinalityLessThan(maxTotalGuardsSize)

            /* Initial negative constraints */
            updateNegativeReduction()

        }
        val nvarDiff = solver.numberOfVariables - nvarStart
        val nconDiff = solver.numberOfClauses - nconStart
        log.info(
            "CompleteTask: Done declaring variables ($nvarDiff) and constraints ($nconDiff) in %.2f s.".format(
                timeDeclare.seconds
            )
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

                fun inputChoice(
                    u: Int,
                    inOldNegUIs: (Int) -> Literal,
                    inPosUIs: (Int) -> Literal
                ): Literal =
                    when (val input = negUIs[u - 1]) {
                        in oldNegUIs -> inOldNegUIs(getOldNegU(input))
                        in posUIs -> inPosUIs(getPosU(input))
                        else -> newLiteral()
                    }

                fun inputChoice(
                    u: Int,
                    inOldNegUIs: (Int) -> IntVar,
                    inPosUIs: (Int) -> IntVar,
                    new: () -> IntVar
                ): IntVar =
                    when (val input = negUIs[u - 1]) {
                        in oldNegUIs -> inOldNegUIs(getOldNegU(input))
                        in posUIs -> inPosUIs(getPosU(input))
                        else -> new()
                    }

                val oldOnlyNegUIs = onlyNegUIs
                onlyNegUIs = negUIs - posUIs
                val newOnlyNegUIs = onlyNegUIs - oldOnlyNegUIs
                val newOnlyNegUs = newOnlyNegUIs.map(::getNegU)

                /* Variables */
                negTransitionFiring = newBoolVarArray(C, K, negU) { (c, k, u) ->
                    inputChoice(u,
                        { negTransitionFiring[c, k, it] },
                        { transitionFiring[c, k, it] })
                }
                negFirstFired = IntVarArray.create(C, negU) { (c, u) ->
                    inputChoice(u,
                        { negFirstFired[c, it] },
                        { firstFired[c, it] },
                        { newIntVar(0..K) })
                }
                negNotFired = newBoolVarArray(C, K, negU) { (c, k, u) ->
                    inputChoice(u,
                        { negNotFired[c, k, it] },
                        { notFired[c, k, it] })
                }
                negActualTransitionFunction = IntVarArray.create(C, E, negU) { (c, e, u) ->
                    inputChoice(u,
                        { negActualTransitionFunction[c, e, it] },
                        { actualTransitionFunction[c, e, it] },
                        { newIntVar(0..C) })
                }
                negNodeValue = newBoolVarArray(C, K, P, negU) { (c, k, p, u) ->
                    if (p == 1) negTransitionFiring[c, k, u]
                    else inputChoice(u,
                        { negNodeValue[c, k, p, it] },
                        { nodeValue[c, k, p, it] })
                }
                negMapping = IntVarArray.create(negV) { (v) ->
                    if (v in newNegVs) newIntVar(0..C)
                    else negMapping[v]
                }

                /* Constraints */
                declareNegativeAutomatonStructureConstraints(vars, Us = newOnlyNegUs)
                declareNegativeGuardConditionsConstraints(vars, Us = newOnlyNegUs)
                declareNegativeMappingConstraints(vars, Vs = newNegVs)
            }
        }
    }

    fun updateCardinalityLessThan(newMaxTotalGuardsSize: Int?) {
        extendedTask.updateCardinalityLessThan(newMaxTotalGuardsSize)
    }

    fun infer(): Automaton? {
        val rawAssignment = solver.solve()
        if (autoFinalize) finalize2()
        if (rawAssignment == null) return null

        val assignment = ExtendedAssignment.fromRaw(rawAssignment, extendedTask.vars)
        val automaton = assignment.toAutomaton()

        with(vars) {
            check(
                automaton.checkMapping(
                    scenarios = scenarioTree.scenarios,
                    mapping = assignment.mapping
                )
            ) { "Positive mapping mismatch" }
            // check(
            automaton.checkMapping(
                scenarios = negativeScenarioTree.negativeScenarios,
                mapping = negMapping.convert(rawAssignment)
            )
            // ) { "Negative mapping mismatch" }
        }

        return automaton
    }

    fun finalize2() {
        extendedTask.finalize2()
        // Note: extendedTask already finalizes the solver, so it is not necessary to do it here again
        // solver.finalize2()
    }
}
