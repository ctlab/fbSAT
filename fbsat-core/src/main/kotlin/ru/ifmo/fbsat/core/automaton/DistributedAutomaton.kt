package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import com.github.lipen.multiarray.mapIndexed
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.MultiScenario
import ru.ifmo.fbsat.core.scenario.MultiScenarioTree
import ru.ifmo.fbsat.core.scenario.PositiveMultiScenario
import ru.ifmo.fbsat.core.scenario.modularInputActionsSeq
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.mutableListOfNulls


class DistributedAutomaton(
    val modules: MultiArray<Automaton>
) {
    val M: Int = modules.shape[0]

    fun eval(
        modularInputAction: MultiArray<InputAction>,
        modularEvalState: MultiArray<Automaton.EvalState>
    ): MultiArray<Automaton.EvalResult> =
        modularEvalState.mapIndexed { (m), state ->
            state.eval(modularInputAction[m])
        }

    fun eval(
        modularInputActions: Sequence<MultiArray<InputAction>>,
        startModularEvalState: MultiArray<Automaton.EvalState>
    ): Sequence<MultiArray<Automaton.EvalResult>> {
        var currentModularEvalState = startModularEvalState
        return modularInputActions.map { modularInputAction ->
            eval(modularInputAction, currentModularEvalState).also { modularResult ->
                currentModularEvalState = modularResult.map { Automaton.EvalState(it) }
            }
        }
    }

    fun eval(
        modularInputActions: Sequence<MultiArray<InputAction>>,
        modularStartState: MultiArray<Automaton.State> = modules.map { it.initialState },
        modularStartOutputValues: MultiArray<OutputValues> = modules.map { Globals.INITIAL_OUTPUT_VALUES }
    ): Sequence<MultiArray<Automaton.EvalResult>> =
        eval(modularInputActions, MultiArray.create(M) { (m) ->
            Automaton.EvalState(modularStartState[m], modularStartOutputValues[m])
        })

    fun eval(
        modularInputActions: Iterable<MultiArray<InputAction>>,
        modularStartState: MultiArray<Automaton.State> = modules.map { it.initialState },
        modularStartOutputValues: MultiArray<OutputValues> = modules.map { Globals.INITIAL_OUTPUT_VALUES }
    ): List<MultiArray<Automaton.EvalResult>> =
        eval(modularInputActions.asSequence(), modularStartState, modularStartOutputValues).toList()

    /**
     * Evaluate given [scenario].
     */
    fun eval(scenario: MultiScenario): Sequence<MultiArray<Automaton.EvalResult>> =
        eval(scenario.modularInputActionsSeq)

    @Suppress("FunctionName")
    fun map_(scenario: MultiScenario): List<MultiArray<Automaton.EvalResult>?> {
        val modularMapping: MutableList<MultiArray<Automaton.EvalResult>?> = mutableListOfNulls(scenario.elements.size)
        out@ for ((i, modularResult) in eval(scenario).withIndex()) {
            val multiElement = scenario.elements[i]
            for (m in 1..M) {
                if (multiElement[m].outputAction != modularResult[m].outputAction) {
                    log.error("No mapping for m = $m, multiElement = ${multiElement.values}, modularResult = $modularResult")
                    break@out
                }
            }
            modularMapping[i] = modularResult
        }
        return modularMapping
    }

    /**
     * Map given [scenario].
     * Nulls represent the absence of a mapping.
     */
    fun map(scenario: MultiScenario): List<MultiArray<Automaton.State>?> =
        map_(scenario).map { modularResult ->
            modularResult?.map { it.destination }
        }

    /**
     * Verify given [positiveMultiScenario].
     *
     * @return `true` if [positiveMultiScenario] is satisfied.
     */
    fun verify(positiveMultiScenario: PositiveMultiScenario): Boolean {
        // old:
        // val results: List<MultiArray<Automaton.EvalResult>?> = eval(positiveMultiScenario)
        // return results.last() != null
        // new:
        val mapping: List<MultiArray<Automaton.State>?> = map(positiveMultiScenario)
        return mapping.last() != null
    }

    /**
     * Verify all positive scenarios in given [multiScenarioTree].
     *
     * @return `true` if **all** scenarios are satisfied.
     */
    fun verify(multiScenarioTree: MultiScenarioTree): Boolean =
        multiScenarioTree.scenarios.all(::verify)
}
