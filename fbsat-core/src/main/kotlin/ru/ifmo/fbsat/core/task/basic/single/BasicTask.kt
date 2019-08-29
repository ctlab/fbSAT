package ru.ifmo.fbsat.core.task.basic.single

import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.constraints.declareAlgorithmConstraints
import ru.ifmo.fbsat.core.constraints.declareAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareColorConstraints
import ru.ifmo.fbsat.core.constraints.declareFiringConstraints
import ru.ifmo.fbsat.core.constraints.declareOutputEventConstraints
import ru.ifmo.fbsat.core.constraints.declareTransitionConstraints
import ru.ifmo.fbsat.core.constraints.declareTransitionsOrderConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.utils.Globals
import java.io.File

@Suppress("MemberVisibilityCanBePrivate", "LocalVariableName")
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

    init {
        with(solver) {
            // Constants
            val C = numberOfStates
            val K = maxOutgoingTransitions ?: C
            val V = scenarioTree.size
            val E = scenarioTree.inputEvents.size
            val O = scenarioTree.outputEvents.size
            val X = scenarioTree.inputNames.size
            val Z = scenarioTree.outputNames.size
            val U = scenarioTree.uniqueInputs.size

            // Variables
            val transition = newArray(C, K, C + 1)
            val actualTransition = newArray(C, E, U, C + 1)
            val inputEvent = newArray(C, K, E + 1)
            val outputEvent = newArray(C, O + 1)
            val algorithm0 = newArray(C, Z)
            val algorithm1 = newArray(C, Z)
            val color = newArray(V, C)
            val rootValue = newArray(C, K, U)
            val firstFired = newArray(C, U, K + 1)
            val notFired = newArray(C, U, K)

            vars = BasicVariables(
                scenarioTree = scenarioTree,
                C = C, K = K, V = V, E = E, O = O, X = X, Z = Z, U = U,
                transition = transition,
                actualTransition = actualTransition,
                inputEvent = inputEvent,
                outputEvent = outputEvent,
                algorithm0 = algorithm0,
                algorithm1 = algorithm1,
                color = color,
                rootValue = rootValue,
                firstFired = firstFired,
                notFired = notFired
            )

            // Constraints
            declareColorConstraints(
                scenarioTree = scenarioTree,
                C = C, K = K, V = V,
                color = color,
                transition = transition,
                actualTransition = actualTransition,
                isEncodeReverseImplication = isEncodeReverseImplication
            )
            declareTransitionConstraints(
                C = C, K = K, E = E, U = U,
                transition = transition,
                actualTransition = actualTransition,
                inputEvent = inputEvent,
                firstFired = firstFired
            )
            declareFiringConstraints(
                C = C, K = K, U = U,
                rootValue = rootValue,
                firstFired = firstFired,
                notFired = notFired
            )
            declareOutputEventConstraints(
                scenarioTree = scenarioTree,
                C = C, O = O,
                color = color,
                outputEvent = outputEvent
            )
            declareAlgorithmConstraints(
                scenarioTree = scenarioTree,
                C = C, Z = Z,
                color = color,
                algorithm0 = algorithm0,
                algorithm1 = algorithm1
            )
            if (Globals.IS_BFS_AUTOMATON) {
                declareAutomatonBfsConstraints(
                    C = C, K = K,
                    transition = transition
                )
            }
            if (Globals.IS_ENCODE_TRANSITIONS_ORDER) {
                declareTransitionsOrderConstraints(
                    C = C, K = K,
                    transition = transition
                )
            }
            declareAdhocConstraints()
        }

        updateCardinality(maxTransitions)
    }

    private fun Solver.declareAdhocConstraints() {
        // TODO: basic adhoc constraints
    }

    fun updateCardinality(newMaxTransitions: Int?) {
        with(vars) {
            solver.updateCardinality(newMaxTransitions)
        }
    }

    fun infer(): Automaton? {
        val rawAssignment = solver.solve()?.data
        if (autoFinalize) finalize2()
        return rawAssignment?.let { raw ->
            BasicAssignment.fromRaw(raw, vars).toAutomaton()
        }
    }

    fun finalize2() {
        solver.finalize2()
    }
}
