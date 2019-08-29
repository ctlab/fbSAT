package ru.ifmo.fbsat.core.task.basic.modular.parallel

import ru.ifmo.fbsat.core.automaton.ModularAutomaton
import ru.ifmo.fbsat.core.constraints.declareParallelModularAlgorithmConstraints
import ru.ifmo.fbsat.core.constraints.declareParallelModularColorConstraints
import ru.ifmo.fbsat.core.constraints.declareParallelModularFiringConstraints
import ru.ifmo.fbsat.core.constraints.declareParallelModularOutputEventConstraints
import ru.ifmo.fbsat.core.constraints.declareParallelModularTransitionConstraints
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import java.io.File

@Suppress("MemberVisibilityCanBePrivate")
class ParallelModularBasicTask(
    val scenarioTree: ScenarioTree,
    val numberOfModules: Int, // M
    val numberOfStates: Int, // C
    maxOutgoingTransitions: Int? = null, // K, =C if null
    maxTransitions: Int? = null, // T, unconstrained if null
    val outDir: File,
    val solver: Solver,
    val autoFinalize: Boolean = true,
    val isEncodeReverseImplication: Boolean = true
) {
    val maxOutgoingTransitions: Int = maxOutgoingTransitions ?: numberOfStates
    val vars: ParallelModularBasicVariables

    init {
        vars = solver.declareVars()
        solver.declareConstraints()
        // updateCardinality(maxTransitions)
    }

    private fun Solver.declareVars() = with(ParallelModularBasicVariables) {
        declare(
            scenarioTree = scenarioTree,
            M = numberOfModules,
            C = numberOfStates,
            K = maxOutgoingTransitions
        )
    }

    private fun Solver.declareConstraints() {
        with(vars) {
            declareParallelModularColorConstraints(
                scenarioTree = scenarioTree,
                M = M, C = C, K = K, V = V,
                colorModular = colorModular,
                transitionModular = transitionModular,
                actualTransitionModular = actualTransitionModular,
                isEncodeReverseImplication = isEncodeReverseImplication
            )
            declareParallelModularTransitionConstraints(
                M = M, C = C, K = K, E = E, U = U,
                transitionModular = transitionModular,
                actualTransitionModular = actualTransitionModular,
                inputEventModular = inputEventModular,
                firstFiredModular = firstFiredModular
            )
            declareParallelModularFiringConstraints(
                M = M, C = C, K = K, U = U,
                rootValueModular = rootValueModular,
                firstFiredModular = firstFiredModular,
                notFiredModular = notFiredModular
            )
            declareParallelModularOutputEventConstraints(
                scenarioTree = scenarioTree,
                M = M, C = C, O = O,
                colorModular = colorModular,
                outputEventModular = outputEventModular
            )
            declareParallelModularAlgorithmConstraints(
                scenarioTree = scenarioTree,
                M = M, C = C, Z = Z,
                outputVariableModule = outputVariableModule,
                algorithm0Modular = algorithm0Modular,
                algorithm1Modular = algorithm1Modular,
                colorModular = colorModular
            )
            // declareModularAutomatonBfsConstraints()
            // if (Globals.IS_ENCODE_TRANSITIONS_ORDER)
            //     declareModularTransitionsOrderConstraints()
            // declareAdhocConstraints()
        }
    }

    private fun Solver.declareAdhocConstraints() {
        TODO()
    }

    fun updateCardinality(newMaxTransitions: Int?) {
        with(vars) {
            solver.updateCardinality(newMaxTransitions)
        }
    }

    fun infer(): ModularAutomaton? {
        val rawAssignment = solver.solve()?.data
        if (autoFinalize) finalize2()
        return rawAssignment?.let { raw ->
            ParallelModularBasicAssignment.fromRaw(raw, vars).toAutomaton()
        }
    }

    fun finalize2() {
        solver.finalize2()
    }
}
