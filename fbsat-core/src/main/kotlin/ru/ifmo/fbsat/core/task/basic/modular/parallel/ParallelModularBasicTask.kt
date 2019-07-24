package ru.ifmo.fbsat.core.task.basic.modular.parallel

import ru.ifmo.fbsat.core.automaton.ModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.declareParallelModularAlgorithmConstraints
import ru.ifmo.fbsat.core.task.declareParallelModularColorConstraints
import ru.ifmo.fbsat.core.task.declareParallelModularFiringConstraints
import ru.ifmo.fbsat.core.task.declareParallelModularOutputEventConstraints
import ru.ifmo.fbsat.core.task.declareParallelModularTransitionConstraints
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
            declareParallelModularTransitionConstraints()
            declareParallelModularFiringConstraints()
            declareParallelModularOutputEventConstraints()
            declareParallelModularAlgorithmConstraints()
            // declareModularAutomatonBfsConstraints()
            // if (Globals.IS_ENCODE_TRANSITIONS_ORDER)
            //     declareModularTransitionsOrderConstraints()
            // TODO: declareAdhocConstraints()
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

    // @Suppress("LocalVariableName")
    // private fun Solver.declareCardinality() {
    //     if (maxTransitions == null) return
    //
    //     val T: Int by context(maxTransitions)
    //     val totalizer: IntArray = context.computeIfAbsent("_totalizerBasic") {
    //         val transition: IntMultiArray by context
    //         declareTotalizer(sequence {
    //             val (M, C, K, _) = transition.shape
    //             for (m in 1..M)
    //                 for (c in 1..C)
    //                     for (k in 1..K)
    //                         yield(-transition[m, c, k, C + 1])
    //         })
    //     } as IntArray
    //     val declaredT: Int? = context["_declaredT"] as Int?
    //     solver.declareComparatorLessThanOrEqual(totalizer, T, declaredT)
    //     context["_declaredT"] = T
    // }
    //
    // private fun Solver.updateCardinality(newMaxTransitions: Int?) {
    //     val vars = Unit
    //     with(vars) {
    //         maxTransitions?.let { T ->
    //             check(newMaxTransitions != null && newMaxTransitions <= T) { "Cannot soften UB" }
    //         }
    //
    //         if (newMaxTransitions == null && !ru.ifmo.fbsat.core.utils.Globals.IS_ENCODE_TOTALIZER) return
    //         if (totalizer == null) {
    //             totalizer = declareTotalizer(sequence {
    //                 for (m in 1..M)
    //                     for (c in 1..C)
    //                         for (k in 1..K)
    //                             yield(-transition[m, c, k, C + 1])
    //             })
    //         }
    //         if (newMaxTransitions == null) return
    //
    //         declareComparatorLessThanOrEqual(totalizer!!, newMaxTransitions, maxTransitions)
    //         maxTransitions = newMaxTransitions
    //     }
    // }
}
