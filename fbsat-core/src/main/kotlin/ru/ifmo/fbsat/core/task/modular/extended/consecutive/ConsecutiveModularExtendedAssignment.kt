package ru.ifmo.fbsat.core.task.modular.extended.consecutive

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.OldPositiveScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.solver.convert
import ru.ifmo.fbsat.core.task.single.extended.ExtendedAssignment
import ru.ifmo.fbsat.core.task.single.extended.toAutomaton

@Suppress("PropertyName")
class ConsecutiveModularExtendedAssignment(
    val scenarioTree: OldPositiveScenarioTree,
    val M: Int,
    val C: Int,
    val K: Int,
    val P: Int,
    val V: Int,
    val E: Int,
    val O: Int,
    val X: Int,
    val Z: Int,
    val U: Int,
    /* Modularized ExtendedAssignment */
    val modularExtendedAssignment: MultiArray<ExtendedAssignment>,
    /* Modular variables */
    val modularComputedOutputValue: MultiArray<BooleanMultiArray>
) {
    /**
     * Number of transitions.
     */
    @Suppress("PropertyName")
    val T: Int = modularExtendedAssignment.values.sumBy { it.T }

    /**
     * Total guards size (total number of nodes in all parse trees).
     */
    @Suppress("PropertyName")
    val N: Int = modularExtendedAssignment.values.sumBy { it.N }

    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(
            raw: RawAssignment,
            vars: ConsecutiveModularExtendedVariables
        ): ConsecutiveModularExtendedAssignment = with(vars) {
            ConsecutiveModularExtendedAssignment(
                scenarioTree = scenarioTree,
                M = M, C = C, K = K, P = P,
                V = V, E = E, O = O, X = X, Z = Z, U = U,
                modularExtendedAssignment = MultiArray.create(M) { (m) ->
                    ExtendedAssignment.fromRaw(raw, modularExtendedVariables[m])
                },
                modularComputedOutputValue = MultiArray.create(M) { (m) ->
                    modularComputedOutputValue[m].convert(raw)
                }
            )
        }
    }
}

@Suppress("LocalVariableName")
fun ConsecutiveModularExtendedAssignment.toAutomaton(): ConsecutiveModularAutomaton {
    val modules = MultiArray.create(M) { (m) ->
        modularExtendedAssignment[m].toAutomaton()
    }

    return ConsecutiveModularAutomaton(modules, scenarioTree)
}
