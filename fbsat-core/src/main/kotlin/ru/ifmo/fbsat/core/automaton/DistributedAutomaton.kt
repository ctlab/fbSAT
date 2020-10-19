package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import com.github.lipen.multiarray.mapIndexed
import ru.ifmo.fbsat.core.scenario.CompoundScenario
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.modularInputActionsSeq
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.solver.SolverContext
import ru.ifmo.fbsat.core.utils.Compound
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.M
import ru.ifmo.fbsat.core.utils.ModularAutomaton
import ru.ifmo.fbsat.core.utils.ModularEvalResult
import ru.ifmo.fbsat.core.utils.ModularEvalResult_adhoc
import ru.ifmo.fbsat.core.utils.ModularEvalState
import ru.ifmo.fbsat.core.utils.ModularInputAction
import ru.ifmo.fbsat.core.utils.ModularOutputAction
import ru.ifmo.fbsat.core.utils.ModularOutputValues
import ru.ifmo.fbsat.core.utils.ModularScenarioTree
import ru.ifmo.fbsat.core.utils.ModularState
import ru.ifmo.fbsat.core.utils.log
import ru.ifmo.fbsat.core.utils.mutableListOfNulls
import ru.ifmo.fbsat.core.utils.toImmutable
import ru.ifmo.fbsat.core.utils.toMultiArray
import java.io.File

@Suppress("MemberVisibilityCanBePrivate", "FunctionName", "PropertyName")
class DistributedAutomaton(
    val modules: ModularAutomaton
) : CompoundImpl<Automaton>() {
    override val modular: ImmutableMultiArray<Automaton> = modules.toImmutable()
    val modularInputEvents: MultiArray<List<InputEvent>> = modules.map { it.inputEvents }
    val modularOutputEvents: MultiArray<List<OutputEvent>> = modules.map { it.outputEvents }
    val modularInputNames: MultiArray<List<String>> = modules.map { it.inputNames }
    val modularOutputNames: MultiArray<List<String>> = modules.map { it.outputNames }

    val numberOfModules: Int = M
    val numberOfStates: Int = modules.values.sumBy { it.numberOfStates }
    val numberOfReachableStates: Int = modules.values.sumBy { it.numberOfReachableStates }
    val numberOfTransitions: Int = modules.values.sumBy { it.numberOfTransitions }
    val totalGuardsSize: Int = modules.values.sumBy { it.totalGuardsSize }

    class CompoundEvalState private constructor(
        val modularState: ModularState,
        val modularOutputValues: ModularOutputValues,
        override val modular: ImmutableMultiArray<Automaton.EvalState>
    ) : Compound<Automaton.EvalState> {
        constructor(modularEvalState: ModularEvalState) : this(
            modularState = modularEvalState.map { it.state },
            modularOutputValues = modularEvalState.map { it.outputValues },
            modular = modularEvalState.toImmutable()
        )

        constructor(
            M: Int,
            modularState: ModularState,
            modularOutputValues: ModularOutputValues
        ) : this(
            modularState = modularState,
            modularOutputValues = modularOutputValues,
            modular = MultiArray.create(M) { (m) ->
                Automaton.EvalState(modularState[m], modularOutputValues[m])
            }.toImmutable()
        )

        fun eval(modularInputAction: ModularInputAction): CompoundEvalResult =
            CompoundEvalResult(
                modular.toMultiArray().mapIndexed { (m), evalState ->
                    evalState.eval(modularInputAction[m])
                }
            )

        fun eval(modularInputActions: Sequence<ModularInputAction>): Sequence<CompoundEvalResult> {
            var currentEvalState = this
            return modularInputActions.map { modularInputAction ->
                currentEvalState.eval(modularInputAction).also {
                    currentEvalState = it.newEvalState
                }
            }
        }
    }

    class CompoundEvalResult private constructor(
        val modularDestination: ModularState,
        val modularOutputAction: ModularOutputAction,
        override val modular: ModularEvalResult_adhoc
    ) : Compound<Automaton.EvalResult> {
        val modularOutputValues: ModularOutputValues = modularOutputAction.map { it.values }
        val newEvalState: CompoundEvalState =
            CompoundEvalState(M, modularDestination, modularOutputValues)

        constructor(modularResult: ModularEvalResult) : this(
            modularDestination = modularResult.map { it.destination },
            modularOutputAction = modularResult.map { it.outputAction },
            modular = modularResult.toImmutable()
        )

        constructor(
            M: Int,
            modularDestination: ModularState,
            modularOutputAction: ModularOutputAction
        ) : this(
            modularDestination = modularDestination,
            modularOutputAction = modularOutputAction,
            modular = MultiArray.create(M) { (m) ->
                Automaton.EvalResult(modularDestination[m], modularOutputAction[m])
            }.toImmutable()
        )

        override fun toString(): String {
            return "CompoundEvalResult(destination = ${modularDestination.values}, outputAction = ${modularOutputAction.values})"
        }
    }

    @Deprecated(
        "Use evalState.eval directly",
        ReplaceWith("compoundEvalState.eval(modularInputAction)"),
        level = DeprecationLevel.ERROR
    )
    fun eval(
        modularInputAction: ModularInputAction,
        evalState: CompoundEvalState
    ): CompoundEvalResult =
        evalState.eval(modularInputAction)

    @Deprecated(
        "Use evalState.eval directly",
        ReplaceWith("startEvalState.eval(modularInputActions)"),
        level = DeprecationLevel.ERROR
    )
    fun eval(
        modularInputActions: Sequence<ModularInputAction>,
        startEvalState: CompoundEvalState
    ): Sequence<CompoundEvalResult> =
        startEvalState.eval(modularInputActions)

    fun eval(
        modularInputActions: Sequence<ModularInputAction>,
        modularStartState: ModularState = modules.map { it.initialState },
        modularStartOutputValues: ModularOutputValues = modules.map { Globals.INITIAL_OUTPUT_VALUES }
    ): Sequence<CompoundEvalResult> =
        CompoundEvalState(M, modularStartState, modularStartOutputValues).eval(modularInputActions)

    fun eval(
        modularInputActions: Iterable<ModularInputAction>,
        modularStartState: ModularState = modules.map { it.initialState },
        modularStartOutputValues: ModularOutputValues = modules.map { Globals.INITIAL_OUTPUT_VALUES }
    ): List<CompoundEvalResult> =
        eval(modularInputActions.asSequence(), modularStartState, modularStartOutputValues).toList()

    /**
     * Evaluate the given [scenario].
     */
    fun eval(scenario: CompoundScenario<*>): Sequence<CompoundEvalResult> =
        eval(scenario.modularInputActionsSeq)

    fun map_(scenario: CompoundScenario<*>): List<CompoundEvalResult?> {
        val mapping: MutableList<CompoundEvalResult?> = mutableListOfNulls(scenario.elements.size)
        out@ for ((i, result) in eval(scenario).withIndex()) {
            val element = scenario.elements[i]
            for (m in 1..M) {
                if (element.modular.toMultiArray()[m].outputAction != result.modular.toMultiArray()[m].outputAction) {
                    // log.error("No mapping for m = $m, element = $element, result = $result")
                    break@out
                }
            }
            mapping[i] = result
        }
        return mapping
    }

    /**
     * Map the given [scenario].
     * Nulls represent the absence of a mapping.
     */
    fun map(scenario: CompoundScenario<*>): List<ModularState?> =
        map_(scenario).map { result ->
            result?.modularDestination
        }

    /**
     * Verify the given [positiveCompoundScenario].
     *
     * @return `true` if [positiveCompoundScenario] is satisfied.
     */
    fun verify(positiveCompoundScenario: PositiveCompoundScenario): Boolean {
        val mapping: List<ModularState?> = map(positiveCompoundScenario)
        return mapping.last() != null
    }

    /**
     * Verify the given [negativeCompoundScenario].
     *
     * @return `true` if [negativeCompoundScenario] is satisfied.
     */
    fun verify(negativeCompoundScenario: NegativeCompoundScenario): Boolean {
        val mapping: List<ModularState?> = map(negativeCompoundScenario)
        if (negativeCompoundScenario.loopPosition != null) {
            val loop = mapping[negativeCompoundScenario.loopPosition - 1]
            val last = mapping.last()
            // log.debug { "loop = ${loop?.values}" }
            // log.debug { "last = ${last?.values}" }
            return when {
                loop == null || last == null -> {
                    // Either `loop` or `last` elements are unmapped,
                    // which means that the negative scenario is not satisfied.
                    // log.debug { "Either 'loop' or 'last' element is unmapped" }
                    true
                }
                loop.values.map { it.id } == last.values.map { it.id } -> {
                    // Both `loop` and `last` elements map to the same state,
                    // which means that the negative scenario is satisfied.
                    log.error("Negative scenario is satisfied (loop==last)")
                    // for ((i, state) in mapping.withIndex(start = 1)) {
                    //     log.debug { "$i -> ${state?.values}" }
                    // }
                    false
                }
                else -> {
                    // `loop` and `last` elements map to different states,
                    // which means that the negative scenario is not satisfied.
                    // log.debug { "Both 'loop' and 'last' elements map to different states" }
                    true
                }
            }
        } else {
            log.warn("Loopless negative scenario?")
            val last = mapping.last()
            return if (last != null) {
                // Satisfaction of the terminal (`last`) element of loop-less negative scenario
                // implies the satisfaction of the negative scenario itself.
                log.error("Negative scenario is satisfied (terminal)")
                false
            } else {
                // Terminal (`last`) element of loop-less negative scenario is unmapped,
                // which means that the negative scenario is not satisfied.
                // log.debug { "Terminal 'last' element is unmapped" }
                true
            }
        }
    }

    /**
     * Verify all positive scenarios in the given [scenarioTree].
     *
     * @return `true` if **all** scenarios are satisfied.
     */
    fun verify(scenarioTree: PositiveCompoundScenarioTree): Boolean =
        scenarioTree.scenarios.all(::verify)

    /**
     * Verify all negative scenarios in the given [scenarioTree].
     *
     * @return `true` if **all** scenarios are **not** satisfied.
     */
    fun verify(scenarioTree: NegativeCompoundScenarioTree): Boolean =
        scenarioTree.scenarios.all(::verify)

    /**
     * Verify all positive scenarios in the given [modularScenarioTree].
     *
     * @return `true` if **all** scenarios are satisfied.
     */
    fun verify(modularScenarioTree: ModularScenarioTree): Boolean {
        for (m in 1..M) {
            if (!modules[m].verify(modularScenarioTree[m])) {
                log.error("Scenario tree verification failed for m = $m")
                return false
            }
        }
        return true
    }

    fun dumpSmv(outDir: File, modularModuleName: MultiArray<String>) {
        for (m in 1..M) {
            modules[m].dumpSmv(
                file = outDir.resolve(modularModuleName[m] + ".smv"),
                name = modularModuleName[m]
            )
        }
    }
}

fun buildBasicDistributedAutomaton(
    context: SolverContext,
    raw: RawAssignment
): DistributedAutomaton {
    val modularContext: MultiArray<SolverContext> by context
    val modules: MultiArray<Automaton> = modularContext.map { ctx ->
        buildBasicAutomaton(
            context = ctx,
            raw = raw,
            useStateUsed = true
        )
    }
    return DistributedAutomaton(modules)
}

fun buildExtendedDistributedAutomaton(
    context: SolverContext,
    raw: RawAssignment
): DistributedAutomaton {
    val modularContext: MultiArray<SolverContext> by context
    val modules: MultiArray<Automaton> = modularContext.map { ctx ->
        buildExtendedAutomaton(
            context = ctx,
            raw = raw,
            useStateUsed = true
        )
    }
    return DistributedAutomaton(modules)
}
