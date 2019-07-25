package ru.ifmo.fbsat.core.task.complete.single

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.extended.single.ExtendedAssignment
import ru.ifmo.fbsat.core.task.extended.single.ExtendedTask
import ru.ifmo.fbsat.core.task.extended.single.toAutomaton
import java.io.File

@Suppress("MemberVisibilityCanBePrivate", "LocalVariableName")
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
        maxTotalGuardsSize = maxTotalGuardsSize,
        outDir = outDir,
        solver = solver,
        autoFinalize = false,
        isEncodeReverseImplication = false
    )

    init {
        vars = CompleteVariables(
            extVars = extendedTask.vars,
            negativeScenarioTree = negativeScenarioTree ?: NegativeScenarioTree(
                inputEvents = scenarioTree.inputEvents,
                outputEvents = scenarioTree.outputEvents,
                inputNames = scenarioTree.inputNames,
                outputNames = scenarioTree.outputNames
            )
        )

        update()
    }

    fun update() {
        with(vars) {
            solver.updateNegativeReduction()
        }
    }

    fun updateCardinality(newMaxTotalGuardsSize: Int?) {
        extendedTask.updateCardinality(newMaxTotalGuardsSize)
        // with(vars) {
        //     solver.updateCardinality(newMaxTotalGuardsSize)
        // }
    }

    fun infer(): Automaton? {
        val rawAssignment = solver.solve()?.data
        if (autoFinalize) finalize2()
        return rawAssignment?.let { raw ->
            ExtendedAssignment.fromRaw(raw, vars).toAutomaton()
        }
    }

    fun finalize2() {
        extendedTask.finalize2()
        // Note: extendedTask already finalizes the solver, so it is not necessary to do it here again
        // solver.finalize2()
    }
}
