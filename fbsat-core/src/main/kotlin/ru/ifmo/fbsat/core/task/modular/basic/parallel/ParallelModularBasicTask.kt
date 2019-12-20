package ru.ifmo.fbsat.core.task.modular.basic.parallel

import com.soywiz.klock.DateTime
import ru.ifmo.fbsat.core.automaton.ModularAutomaton
import ru.ifmo.fbsat.core.constraints.declareParallelModularAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareParallelModularAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveParallelModularMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.core.solver.declareTotalizer
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.secondsSince
import java.io.File

@Suppress("MemberVisibilityCanBePrivate", "LocalVariableName")
class ParallelModularBasicTask(
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
    val vars: ParallelModularBasicVariables

    init {
        val timeStart = DateTime.nowLocal()
        val nvarStart = solver.numberOfVariables
        val nconStart = solver.numberOfClauses

        with(solver) {
            /* Variables */
            vars = declareParallelModularBasicVariables(
                scenarioTree = scenarioTree,
                M = numberOfModules,
                C = numberOfStates,
                K = maxOutgoingTransitions ?: numberOfStates
            )

            /* Constraints */
            declareParallelModularAutomatonStructureConstraints(vars)
            if (Globals.IS_BFS_AUTOMATON) declareParallelModularAutomatonBfsConstraints(vars)
            declarePositiveParallelModularMappingConstraints(vars, isEncodeReverseImplication)
            declareAdhocConstraints()
        }

        /* Initial cardinality constraints */
        updateCardinality(maxTransitions)

        val nvarDiff = solver.numberOfVariables - nvarStart
        val nconDiff = solver.numberOfClauses - nconStart
        log.info(
            "ParallelModularBasicTask: Done declaring variables ($nvarDiff) and constraints ($nconDiff) in %.2f s"
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

    fun infer(): ModularAutomaton? {
        val rawAssignment = solver.solve()
        if (autoFinalize) finalize2()
        return rawAssignment?.let { raw ->
            ParallelModularBasicAssignment.fromRaw(raw, vars).toAutomaton()
        }
    }

    fun finalize2() {
        solver.finalize2()
    }
}
