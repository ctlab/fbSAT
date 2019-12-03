package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import com.github.lipen.multiarray.IntMultiArray
import com.github.lipen.multiarray.MultiArray
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
            /* Constants */
            @Suppress("UnnecessaryVariable")
            val M = numberOfModules
            @Suppress("UnnecessaryVariable")
            val C = numberOfStates
            val K = maxOutgoingTransitions ?: C
            val V = scenarioTree.size
            val E = scenarioTree.inputEvents.size
            val O = scenarioTree.outputEvents.size
            val Z = scenarioTree.outputNames.size
            val U = scenarioTree.uniqueInputs.size

            fun newModularArray(
                vararg shape: Int,
                one: Boolean = false,
                init: (IntArray) -> Int = { newVariable() }
            ): MultiArray<IntMultiArray> =
                MultiArray.create(M) { newArray(*shape, one = one, init = init) }

            /* Core variables */
            val modularTransitionDestination = newModularArray(C, K, C + 1, one = true)
            val modularTransitionInputEvent = MultiArray.create(M) { (m) ->
                if (m == 1)
                    newArray(C, K, E + 1, one = true)
                else
                    newArray(C, K, 1 + 1, one = true)
            }
            val modularTransitionFiring = newModularArray(C, K, U)
            val modularFirstFired = newModularArray(C, U, K + 1)
            val modularNotFired = newModularArray(C, K, U)
            val modularStateOutputEvent = MultiArray.create(M) { (m) ->
                if (m == M)
                    newArray(C, O + 1, one = true)
                else
                    newArray(C, 1 + 1, one = true)
            }
            val modularStateAlgorithmBot = newModularArray(C, Z)
            val modularStateAlgorithmTop = newModularArray(C, Z)
            /* Interface variables */
            val modularActualTransitionFunction = MultiArray.create(M) { (m) ->
                if (m == 1)
                    newArray(C, E, U, C + 1, one = true)
                else
                    newArray(C, 1, U, C + 1, one = true)
            }
            /* Mapping variables */
            val modularMapping = newModularArray(V, C, one = true)
            val modularComputedOutputValue = newModularArray(V, Z)

            vars = ConsecutiveModularBasicVariables(
                scenarioTree = scenarioTree,
                M = M, C = C, K = K,
                modularTransitionDestination = modularTransitionDestination,
                modularTransitionInputEvent = modularTransitionInputEvent,
                modularTransitionFiring = modularTransitionFiring,
                modularFirstFired = modularFirstFired,
                modularNotFired = modularNotFired,
                modularStateOutputEvent = modularStateOutputEvent,
                modularStateAlgorithmTop = modularStateAlgorithmTop,
                modularStateAlgorithmBot = modularStateAlgorithmBot,
                modularActualTransitionFunction = modularActualTransitionFunction,
                modularMapping = modularMapping,
                modularComputedOutputValue = modularComputedOutputValue
            )

            /* Constraints */
            declareConsecutiveModularAutomatonStructureConstraints(vars)
            if (Globals.IS_BFS_AUTOMATON) declareConsecutiveModularAutomatonBfsConstraints(vars)
            declarePositiveConsecutiveModularMappingConstraints(vars, isEncodeReverseImplication)
            declareAdhocConstraints()
        }

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
            for (m in 1..M)
                for (c in 1..C)
                    for (k in 1..K)
                        clause(-modularTransitionDestination[m][c, k, 1])
        }
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
                        for (m in 1..M)
                            for (c in 1..C)
                                for (k in 1..K)
                                    yield(-modularTransitionDestination[m][c, k, C + 1])
                    }
                }
                if (newMaxTransitions == null) return

                declareComparatorLessThanOrEqual(totalizer!!, newMaxTransitions, maxTransitions)
                maxTransitions = newMaxTransitions
            }
        }
    }

    fun infer(): ConsecutiveModularAutomaton? {
        val rawAssignment = solver.solve()?.data
        if (autoFinalize) finalize2()
        return rawAssignment?.let { raw ->
            ConsecutiveModularBasicAssignment.fromRaw(raw, vars).toAutomaton()
        }
    }

    fun finalize2() {
        solver.finalize2()
    }
}
