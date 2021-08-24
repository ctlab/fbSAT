package ru.ifmo.fbsat.core.automaton

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import com.github.lipen.multiarray.mapIndexed
import com.github.lipen.satlib.core.Context
import com.github.lipen.satlib.core.Model
import ru.ifmo.fbsat.core.scenario.CompoundScenario
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.inputActionsSeq
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenario
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveCompoundScenarioTree
import ru.ifmo.fbsat.core.utils.Compound
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.Globals
import ru.ifmo.fbsat.core.utils.ModularAutomaton
import ru.ifmo.fbsat.core.utils.ModularContext
import ru.ifmo.fbsat.core.utils.ModularEvalResult
import ru.ifmo.fbsat.core.utils.ModularEvalState
import ru.ifmo.fbsat.core.utils.ModularInputAction
import ru.ifmo.fbsat.core.utils.ModularOutputAction
import ru.ifmo.fbsat.core.utils.ModularOutputValues
import ru.ifmo.fbsat.core.utils.ModularScenarioTree
import ru.ifmo.fbsat.core.utils.ModularState
import ru.ifmo.fbsat.core.utils.MyLogger
import ru.ifmo.fbsat.core.utils.mutableListOfNulls
import ru.ifmo.fbsat.core.utils.project
import java.io.File

private val logger = MyLogger {}

@Suppress("MemberVisibilityCanBePrivate", "FunctionName", "PropertyName")
class DistributedAutomaton(
    val modules: ModularAutomaton,
) : CompoundImpl<Automaton>() {
    override val modular: MultiArray<Automaton> = modules
    val modularInputEvents: MultiArray<List<InputEvent>> = modules.map { it.inputEvents }
    val modularOutputEvents: MultiArray<List<OutputEvent>> = modules.map { it.outputEvents }
    val modularInputNames: MultiArray<List<String>> = modules.map { it.inputNames }
    val modularOutputNames: MultiArray<List<String>> = modules.map { it.outputNames }

    val numberOfModules: Int = M
    val numberOfStates: Int = modules.values.sumOf { it.numberOfStates }
    val numberOfReachableStates: Int = modules.values.sumOf { it.numberOfReachableStates }
    val numberOfTransitions: Int = modules.values.sumOf { it.numberOfTransitions }
    val totalGuardsSize: Int = modules.values.sumOf { it.totalGuardsSize }

    class CompoundEvalState private constructor(
        val modularState: ModularState,
        val modularOutputValues: ModularOutputValues,
        override val modular: MultiArray<Automaton.EvalState>,
    ) : Compound<Automaton.EvalState> {
        constructor(modularEvalState: ModularEvalState) : this(
            modularState = modularEvalState.map { it.state },
            modularOutputValues = modularEvalState.map { it.outputValues },
            modular = modularEvalState
        )

        constructor(
            M: Int,
            modularState: ModularState,
            modularOutputValues: ModularOutputValues,
        ) : this(
            modularState = modularState,
            modularOutputValues = modularOutputValues,
            modular = MultiArray.new(M) { (m) ->
                Automaton.EvalState(modularState[m], modularOutputValues[m])
            }
        )

        fun eval(modularInputAction: ModularInputAction): CompoundEvalResult =
            CompoundEvalResult(
                modular.mapIndexed { (m), evalState ->
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
        val modularInputAction: ModularInputAction,
        val modularDestination: ModularState,
        val modularOutputAction: ModularOutputAction,
        override val modular: ModularEvalResult,
    ) : CompoundImpl<Automaton.EvalResult>() {
        val modularOutputValues: ModularOutputValues = modularOutputAction.map { it.values }
        val newEvalState: CompoundEvalState = CompoundEvalState(M, modularDestination, modularOutputValues)

        constructor(modularResult: ModularEvalResult) : this(
            modularInputAction = modularResult.map { it.inputAction },
            modularDestination = modularResult.map { it.destination },
            modularOutputAction = modularResult.map { it.outputAction },
            modular = modularResult
        )

        constructor(
            M: Int,
            modularInputAction: ModularInputAction,
            modularDestination: ModularState,
            modularOutputAction: ModularOutputAction,
        ) : this(
            modularInputAction = modularInputAction,
            modularDestination = modularDestination,
            modularOutputAction = modularOutputAction,
            modular = MultiArray.new(M) { (m) ->
                Automaton.EvalResult(
                    inputAction = modularInputAction[m],
                    destination = modularDestination[m],
                    outputAction = modularOutputAction[m]
                )
            }
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
        evalState: CompoundEvalState,
    ): CompoundEvalResult =
        evalState.eval(modularInputAction)

    @Deprecated(
        "Use evalState.eval directly",
        ReplaceWith("startEvalState.eval(modularInputActions)"),
        level = DeprecationLevel.ERROR
    )
    fun eval(
        modularInputActions: Sequence<ModularInputAction>,
        startEvalState: CompoundEvalState,
    ): Sequence<CompoundEvalResult> =
        startEvalState.eval(modularInputActions)

    fun eval(
        modularInputActions: Sequence<ModularInputAction>,
        modularStartState: ModularState = modules.map { it.initialState },
        modularStartOutputValues: ModularOutputValues = modules.map { Globals.INITIAL_OUTPUT_VALUES },
    ): Sequence<CompoundEvalResult> =
        CompoundEvalState(M, modularStartState, modularStartOutputValues).eval(modularInputActions)

    fun eval(
        modularInputActions: Iterable<ModularInputAction>,
        modularStartState: ModularState = modules.map { it.initialState },
        modularStartOutputValues: ModularOutputValues = modules.map { Globals.INITIAL_OUTPUT_VALUES },
    ): List<CompoundEvalResult> =
        eval(modularInputActions.asSequence(), modularStartState, modularStartOutputValues).toList()

    /**
     * Evaluate the given [scenario].
     */
    fun eval(scenario: CompoundScenario<*>): Sequence<CompoundEvalResult> =
        eval(scenario.inputActionsSeq.map { it.modular })

    fun map_(scenario: CompoundScenario<*>): List<CompoundEvalResult?> {
        val mapping: MutableList<CompoundEvalResult?> = mutableListOfNulls(scenario.elements.size)
        out@ for ((i, result) in eval(scenario).withIndex()) {
            val element = scenario.elements[i]
            for (m in 1..M) {
                if (element.project(m).outputAction != result.project(m).outputAction) {
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
                    logger.error("Negative scenario is satisfied (loop==last)")
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
            logger.warn("Loopless negative scenario?")
            val last = mapping.last()
            return if (last != null) {
                // Satisfaction of the terminal (`last`) element of loop-less negative scenario
                // implies the satisfaction of the negative scenario itself.
                logger.error("Negative scenario is satisfied (terminal)")
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
                logger.error("Scenario tree verification failed for m = $m")
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
    context: Context,
    model: Model,
): DistributedAutomaton {
    val modularContext: ModularContext = context["modularContext"]
    val modules: MultiArray<Automaton> = modularContext.map { ctx ->
        buildBasicAutomaton(
            context = ctx,
            model = model,
            useStateUsed = true
        )
    }
    return DistributedAutomaton(modules)
}

fun buildExtendedDistributedAutomaton(
    context: Context,
    model: Model,
): DistributedAutomaton {
    val modularContext: ModularContext = context["modularContext"]
    val modules: MultiArray<Automaton> = modularContext.map { ctx ->
        buildExtendedAutomaton(
            context = ctx,
            model = model,
            useStateUsed = true
        )
    }
    return DistributedAutomaton(modules)
}
