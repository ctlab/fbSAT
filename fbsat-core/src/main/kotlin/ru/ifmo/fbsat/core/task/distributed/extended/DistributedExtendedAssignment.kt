package ru.ifmo.fbsat.core.task.distributed.extended

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.automaton.NodeType
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.task.single.extended.ExtendedAssignment
import ru.ifmo.fbsat.core.task.single.extended.toAutomaton

class DistributedExtendedAssignment(
    /* Constants */
    val M: Int,
    val modularScenarioTree: MultiArray<PositiveScenarioTree>,
    val modularC: MultiArray<Int>,
    val modularK: MultiArray<Int>,
    val modularP: MultiArray<Int>,
    val modularV: MultiArray<Int>,
    val modularE: MultiArray<Int>,
    val modularO: MultiArray<Int>,
    val modularX: MultiArray<Int>,
    val modularZ: MultiArray<Int>,
    val modularU: MultiArray<Int>,
    /* Modularized ExtendedAssignment */
    val modularExtendedAssignment: MultiArray<ExtendedAssignment>
) {
    val modularT: MultiArray<Int> = modularExtendedAssignment.map { it.T }
    val modularN: MultiArray<Int> = modularExtendedAssignment.map { it.N }
    val T: Int = modularExtendedAssignment.values.sumBy { assignment ->
        assignment.transitionDestination.values.count { it != 0 }
    }
    val N: Int = modularExtendedAssignment.values.sumBy { assignment ->
        assignment.nodeType.values.count { it != NodeType.NONE }
    }

    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(
            raw: RawAssignment,
            vars: DistributedExtendedVariables
        ): DistributedExtendedAssignment = with(vars) {
            DistributedExtendedAssignment(
                M = M,
                modularScenarioTree = modularScenarioTree,
                modularC = modularC,
                modularK = modularK,
                modularP = modularP,
                modularV = modularV,
                modularE = modularE,
                modularO = modularO,
                modularX = modularX,
                modularZ = modularZ,
                modularU = modularU,
                modularExtendedAssignment = modularExtendedVariables.map { ExtendedAssignment.fromRaw(raw, it) }
            )
        }
    }
}

fun DistributedExtendedAssignment.toAutomaton(): DistributedAutomaton {
    val modules: MultiArray<Automaton> = modularExtendedAssignment.map { it.toAutomaton() }
    return DistributedAutomaton(modules)
}
