package ru.ifmo.fbsat.core.task.single.basic

import com.soywiz.klock.DateTime
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.constraints.declareAutomatonBfsConstraints
import ru.ifmo.fbsat.core.constraints.declareAutomatonStructureConstraints
import ru.ifmo.fbsat.core.constraints.declarePositiveMappingConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.declareComparatorLessThanOrEqual
import ru.ifmo.fbsat.core.solver.declareTotalizer
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.secondsSince
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
        val timeStart = DateTime.now()

        with(solver) {
            /* Constants */
            val C = numberOfStates
            val K = maxOutgoingTransitions ?: C
            val V = scenarioTree.size
            val E = scenarioTree.inputEvents.size
            val O = scenarioTree.outputEvents.size
            val X = scenarioTree.inputNames.size
            val Z = scenarioTree.outputNames.size
            val U = scenarioTree.uniqueInputs.size

            println("C = $C")
            println("K = $K")
            println("V = $V")
            println("E = $E")
            println("O = $O")
            println("X = $X")
            println("Z = $Z")
            println("U = $U")

            /* Core variables */
            val transitionDestination = newArray(C, K, C + 1, one = true)
            val transitionInputEvent = newArray(C, K, E + 1, one = true)
            val transitionFiring = newArray(C, K, U)
            val firstFired = newArray(C, U, K + 1, one = true)
            val notFired = newArray(C, K, U)
            val stateOutputEvent = newArray(C, O + 1, one = true)
            val stateAlgorithmBot = newArray(C, Z)
            val stateAlgorithmTop = newArray(C, Z)
            /* Interface variables */
            val transitionFunction = newArray(C, E, U, C, one = true)
            val actualTransitionFunction = newArray(C, E, U, C + 1, one = true)
            val outputEventFunction = newArray(C, E, U, O + 1, one = true)
            val algorithmFunctionBot = newArray(C, E, U, Z)
            val algorithmFunctionTop = newArray(C, E, U, Z)
            /* Mapping variables */
            val mapping = newArray(V, C, one = true)
            // val memoryOutputValue = newArray(V, Z)

            vars = BasicVariables(
                scenarioTree = scenarioTree,
                C = C, K = K,
                transitionDestination = transitionDestination,
                transitionInputEvent = transitionInputEvent,
                transitionFiring = transitionFiring,
                firstFired = firstFired,
                notFired = notFired,
                stateOutputEvent = stateOutputEvent,
                stateAlgorithmTop = stateAlgorithmTop,
                stateAlgorithmBot = stateAlgorithmBot,
                transitionFunction = transitionFunction,
                actualTransitionFunction = actualTransitionFunction,
                outputEventFunction = outputEventFunction,
                algorithmFunctionTop = algorithmFunctionTop,
                algorithmFunctionBot = algorithmFunctionBot,
                mapping = mapping
            )

            /* Constraints */
            declareAutomatonStructureConstraints(vars)
            declareAutomatonBfsConstraints(vars)
            declarePositiveMappingConstraints(vars)
            declareAdhocConstraints()
        }

        updateCardinality(maxTransitions)

        log.info("Done declaring variables and constraints in %.2f s".format(secondsSince(timeStart)))
    }

    private fun Solver.declareAdhocConstraints() {
        // TODO: basic adhoc constraints
    }

    fun updateCardinality(newMaxTransitions: Int?) {
        with(solver) {
            with(vars) {
                maxTransitions?.let { T ->
                    check(newMaxTransitions != null && newMaxTransitions <= T) { "Cannot soften UB" }
                }

                if (newMaxTransitions == null && !Globals.IS_ENCODE_TOTALIZER) return
                if (totalizer == null) {
                    totalizer = declareTotalizer {
                        for (c in 1..C)
                            for (k in 1..K)
                                yield(-transitionDestination[c, k, C + 1])
                    }
                }
                if (newMaxTransitions == null) return

                declareComparatorLessThanOrEqual(totalizer!!, newMaxTransitions, maxTransitions)
                maxTransitions = newMaxTransitions
            }
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
