package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import com.soywiz.klock.PerformanceCounter
import ru.ifmo.fbsat.core.automaton.ArbitraryModularAutomaton
import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.constraints.declareArbitraryModularAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareArbitraryModularAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveArbitraryModularMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.timeSince
import java.io.File

@Suppress("LocalVariableName", "MemberVisibilityCanBePrivate")
class ArbitraryModularBasicTask(
    scenarioTree: ScenarioTree,
    numberOfModules: Int, // M
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    val outDir: File,
    val solver: Solver,
    val autoFinalize: Boolean = true,
    isEncodeReverseImplication: Boolean = true
) {
    val maxOutgoingTransitions: Int = maxOutgoingTransitions ?: numberOfStates
    val vars: ArbitraryModularBasicVariables
    internal val cardinality: Cardinality

    init {
        require(numberOfModules >= 2) { "Number of modules must be at least 2" }
        require(scenarioTree.inputEvents == listOf(InputEvent("REQ")))
        require(scenarioTree.outputEvents == listOf(OutputEvent("CNF")))

        val timeStart = PerformanceCounter.reference
        val nvarStart = solver.numberOfVariables
        val nconStart = solver.numberOfClauses

        with(solver) {
            /* Variables */
            vars = declareArbitraryModularBasicVariables(
                scenarioTree = scenarioTree,
                M = numberOfModules,
                C = numberOfStates,
                K = maxOutgoingTransitions ?: numberOfStates
            )

            /* Cardinality */
            cardinality = declareCardinality {
                with(vars) {
                    for (m in 1..M) {
                        for (c in 1..C)
                            for (k in 1..K)
                                yield(modularTransitionDestination[m][c, k] neq 0)
                    }
                }
            }

            /* Constraints */
            declareArbitraryModularAutomatonStructureConstraints(vars)
            if (Globals.IS_BFS_AUTOMATON) declareArbitraryModularAutomatonBfsConstraints(vars)
            declarePositiveArbitraryModularMappingConstraints(vars, isEncodeReverseImplication)
            // declareAdhocConstraints()
        }

        /* Initial cardinality constraints */
        updateCardinalityLessThan(maxTransitions?.let { it + 1 })

        val nvarDiff = solver.numberOfVariables - nvarStart
        val nconDiff = solver.numberOfClauses - nconStart
        log.info(
            "ArbitraryModularBasicTask: Done declaring variables ($nvarDiff) and constraints ($nconDiff) in %.2f s.".format(
                timeSince(timeStart).seconds
            )
        )
    }

    private fun Solver.declareAdhocConstraints() {
        comment("ADHOC constraints")
        with(vars) {
            comment("Ad-hoc: no transition to the first state")
            for (m in 1..M) {
                for (c in 1..C)
                    for (k in 1..K)
                        clause(modularTransitionDestination[m][c, k] neq 1)
            }
        }
    }

    fun updateCardinalityLessThan(newMaxTransitions: Int?) {
        cardinality.updateUpperBoundLessThan(newMaxTransitions)
    }

    fun infer(): ArbitraryModularAutomaton? {
        val rawAssignment = solver.solve()
        if (autoFinalize) finalize2()
        return rawAssignment?.let { raw ->
            ArbitraryModularBasicAssignment.fromRaw(raw, vars).toAutomaton()
        }
    }

    fun finalize2() {
        solver.finalize2()
    }
}
