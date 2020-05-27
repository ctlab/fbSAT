package ru.ifmo.fbsat.core.task.modular.basic.consecutive

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.automaton.ConsecutiveModularAutomaton
import ru.ifmo.fbsat.core.scenario.positive.ScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.solver.convert
import ru.ifmo.fbsat.core.task.single.basic.BasicAssignment
import ru.ifmo.fbsat.core.task.single.basic.toAutomaton

class ConsecutiveModularBasicAssignment(
    val scenarioTree: ScenarioTree,
    /* Constants */
    val M: Int,
    val C: Int,
    val K: Int,
    val V: Int,
    val E: Int,
    val O: Int,
    val X: Int,
    val Z: Int,
    val U: Int,
    /* Modularized BasicAssignment */
    val modularBasicAssignment: MultiArray<BasicAssignment>,
    /* Modular variables */
    val modularComputedOutputValue: MultiArray<BooleanMultiArray>
) {
    /**
     * Number of transitions.
     */
    @Suppress("PropertyName")
    val T: Int = modularBasicAssignment.values.sumBy { it.T }

    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(
            raw: RawAssignment,
            vars: ConsecutiveModularBasicVariables
        ): ConsecutiveModularBasicAssignment = with(vars) {
            ConsecutiveModularBasicAssignment(
                scenarioTree = scenarioTree,
                M = M, C = C, K = K,
                V = V, E = E, O = O, X = X, Z = Z, U = U,
                modularBasicAssignment = MultiArray.create(M) { (m) ->
                    BasicAssignment.fromRaw(raw, modularBasicVariables[m])
                },
                modularComputedOutputValue = MultiArray.create(M) { (m) ->
                    modularComputedOutputValue[m].convert(raw)
                }
            )
        }
    }
}

@Suppress("LocalVariableName")
fun ConsecutiveModularBasicAssignment.toAutomaton(): ConsecutiveModularAutomaton {
    val modules = MultiArray.create(M) { (m) ->
        modularBasicAssignment[m].toAutomaton()
    }

    return ConsecutiveModularAutomaton(modules, scenarioTree)
}
