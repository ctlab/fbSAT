package ru.ifmo.fbsat.core.task.single.basic

import com.github.lipen.satlib.card.Cardinality
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.constraints.declareAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.utils.Globals

data class BasicTask(
    val scenarioTree: PositiveScenarioTree,
    val numberOfStates: Int, // C
    val maxOutgoingTransitions: Int? = null, // K, =C if null
    val maxTransitions: Int? = null, // T, unconstrained if null
    val isEncodeReverseImplication: Boolean = true,
) : Task() {
    override fun Solver.declare_() {
        /* Variables */
        comment("$name: Variables")
        declareBasicVariables(
            positiveScenarioTree = scenarioTree,
            C = numberOfStates,
            K = maxOutgoingTransitions ?: numberOfStates
        )

        /* Constraints */
        comment("$name: Constraints")
        declareAutomatonStructureConstraints()
        if (Globals.IS_BFS_AUTOMATON) declareAutomatonBfsConstraints()
        declarePositiveMappingConstraints(isEncodeReverseImplication = isEncodeReverseImplication)

        /* Initial cardinality constraints */
        comment("$name: Initial cardinality (T) constraints")
        val cardinalityT: Cardinality = context["cardinalityT"]
        // old
        // cardinalityT.declareUpperBoundLessThanOrEqual(maxTransitions)
        // adhoc
        if (Globals.IS_USE_ASSUMPTIONS) {
            cardinalityT.assumeUpperBoundLessThanOrEqual(maxTransitions)
        } else {
            cardinalityT.declareUpperBoundLessThanOrEqual(maxTransitions)
        }

    }
}
