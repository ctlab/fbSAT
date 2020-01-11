package ru.ifmo.fbsat.core.task.single.basic

import com.soywiz.klock.DateTime
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.constraints.declareAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Cardinality
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareCardinality
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.checkMapping
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.secondsSince
import java.io.File

@Suppress("LocalVariableName")
class BasicTask(
    scenarioTree: ScenarioTree,
    numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    val outDir: File,
    val solver: Solver,
    val autoFinalize: Boolean = true,
    isEncodeReverseImplication: Boolean = true
) {
    val vars: BasicVariables
    internal val cardinality: Cardinality

    init {
        val timeStart = DateTime.nowLocal()
        val nvarStart = solver.numberOfVariables
        val nconStart = solver.numberOfClauses

        with(solver) {
            /* Variables */
            vars = declareBasicVariables(
                scenarioTree,
                C = numberOfStates,
                K = maxOutgoingTransitions ?: numberOfStates
            )

            /* Cardinality */
            cardinality = declareCardinality {
                with(vars) {
                    for (c in 1..C)
                        for (k in 1..K)
                            yield(transitionDestination[c, k] neq 0)
                }
            }

            /* Constraints */
            declareAutomatonStructureConstraints(vars)
            if (Globals.IS_BFS_AUTOMATON) declareAutomatonBfsConstraints(vars)
            declarePositiveMappingConstraints(vars, isEncodeReverseImplication = isEncodeReverseImplication)
            declareAdhocConstraints()
        }

        /* Initial cardinality constraints */
        updateCardinalityLessThan(maxTransitions?.let { it + 1 })

        val nvarDiff = solver.numberOfVariables - nvarStart
        val nconDiff = solver.numberOfClauses - nconStart
        log.info(
            "BasicTask: Done declaring variables ($nvarDiff) and constraints ($nconDiff) in %.2f s"
                .format(secondsSince(timeStart))
        )
    }

    private fun Solver.declareAdhocConstraints() {
        comment("ADHOC constraints")
        with(vars) {
            if (Globals.IS_FORBID_TRANSITIONS_TO_FIRST_STATE) {
                comment("Ad-hoc: no transition to the first state")
                for (c in 1..C)
                    for (k in 1..K)
                        clause(transitionDestination[c, k] neq 1)
            }
        }
    }

    fun updateCardinalityLessThan(newMaxTransitions: Int?) {
        cardinality.updateUpperBoundLessThan(newMaxTransitions)
    }

    fun infer(): Automaton? {
        val rawAssignment = solver.solve()
        if (autoFinalize) finalize2()
        if (rawAssignment == null) return null

        val assignment = BasicAssignment.fromRaw(rawAssignment, vars)
        val automaton = assignment.toAutomaton()

        with(vars) {
            check(
                automaton.checkMapping(
                    scenarios = scenarioTree.scenarios,
                    mapping = assignment.mapping
                )
            ) { "Positive mapping mismatch" }
        }

        return automaton
    }

    fun finalize2() {
        solver.finalize2()
    }
}
