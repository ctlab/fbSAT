package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import com.soywiz.klock.DateTime
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareConsecutiveModularAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveConsecutiveModularMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.core.solver.declareTotalizer
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.secondsSince
import java.io.File

@Suppress("LocalVariableName", "MemberVisibilityCanBePrivate")
class ConsecutiveModularBasicTask(
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
    val vars: ConsecutiveModularBasicVariables

    init {
        require(numberOfModules >= 2) { "Number of modules must be at least 2" }
        require(scenarioTree.inputEvents == listOf(InputEvent("REQ")))
        require(scenarioTree.outputEvents == listOf(OutputEvent("CNF")))

        val timeStart = DateTime.nowLocal()
        val nvarStart = solver.numberOfVariables
        val nconStart = solver.numberOfClauses

        with(solver) {
            /* Variables */
            vars = declareConsecutiveModularBasicVariables(
                scenarioTree = scenarioTree,
                M = numberOfModules,
                C = numberOfStates,
                K = maxOutgoingTransitions ?: numberOfStates
            )

            /* Constraints */
            declareConsecutiveModularAutomatonStructureConstraints(vars)
            if (Globals.IS_BFS_AUTOMATON) declareConsecutiveModularAutomatonBfsConstraints(vars)
            declarePositiveConsecutiveModularMappingConstraints(vars, isEncodeReverseImplication)
            declareAdhocConstraints()
        }

        /* Initial cardinality constraints */
        updateCardinality(maxTransitions)

        val nvarDiff = solver.numberOfVariables - nvarStart
        val nconDiff = solver.numberOfClauses - nconStart
        log.info(
            "ConsecutiveModularBasicTask: Done declaring variables ($nvarDiff) and constraints ($nconDiff) in %.2f s"
                .format(secondsSince(timeStart))
        )
    }

    private fun Solver.declareAdhocConstraints() {
        comment("ADHOC constraints")
        with(vars) {
            comment("Ad-hoc: no transition to the first state")
            for (m in 1..M) with(modularBasicVariables[m]) {
                for (c in 1..C)
                    for (k in 1..K)
                        clause(transitionDestination[c, k] neq 1)
            }
        }
    }

    @Suppress("DuplicatedCode")
    fun updateCardinality(newMaxTransitions: Int?) {
        with(solver) {
            with(vars) {
                maxTransitions?.let { T ->
                    check(newMaxTransitions != null && newMaxTransitions <= T) { "Cannot soften UB" }
                }

                if (newMaxTransitions == null && !Globals.IS_ENCODE_TOTALIZER) return
                if (totalizer == null) {
                    totalizer = declareTotalizer {
                        for (m in 1..M) with(modularBasicVariables[m]) {
                            for (c in 1..C)
                                for (k in 1..K)
                                    yield(transitionDestination[c, k] neq 0)
                        }
                    }
                }
                if (newMaxTransitions == null) return

                declareComparatorLessThanOrEqual(totalizer!!, newMaxTransitions, maxTransitions)
                maxTransitions = newMaxTransitions
            }
        }
    }

    fun infer(): ConsecutiveModularAutomaton? {
        val rawAssignment = solver.solve()
        if (autoFinalize) finalize2()
        return rawAssignment?.let { raw ->
            ConsecutiveModularBasicAssignment.fromRaw(raw, vars).toAutomaton()
        }
    }

    fun finalize2() {
        solver.finalize2()
    }
}
