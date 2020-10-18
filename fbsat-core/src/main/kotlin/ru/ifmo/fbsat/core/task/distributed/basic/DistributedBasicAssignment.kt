package ru.ifmo.fbsat.core.task.distributed.basic

import com.github.lipen.multiarray.BooleanMultiArray
import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import com.github.lipen.multiarray.mapIndexed
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.DistributedAutomaton
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.RawAssignment
import ru.ifmo.fbsat.core.solver.convert
import ru.ifmo.fbsat.core.task.single.basic.BasicAssignment
import ru.ifmo.fbsat.core.task.single.basic.toAutomaton

@Suppress("PropertyName")
class DistributedBasicAssignment(
    /* Constants */
    val M: Int,
    val modularScenarioTree: MultiArray<PositiveScenarioTree>,
    val modularC: MultiArray<Int>,
    val modularK: MultiArray<Int>,
    val modularV: MultiArray<Int>,
    val modularE: MultiArray<Int>,
    val modularO: MultiArray<Int>,
    val modularX: MultiArray<Int>,
    val modularZ: MultiArray<Int>,
    val modularU: MultiArray<Int>,
    /* Modularized BasicAssignment */
    val modularBasicAssignment: MultiArray<BasicAssignment>,
    /* Extra */
    val stateUsed: BooleanMultiArray
) {
    val modularT: MultiArray<Int> = modularBasicAssignment.map { it.T }
    val T: Int = modularBasicAssignment.values.sumBy { assignment ->
        assignment.transitionDestination.values.count { it != 0 }
    }

    init {
        println("stateUsed:")
        for (m in 1..M) {
            println(" - [m=$m]: ${(1..modularC[m]).map { c -> stateUsed[m, c] }}")
        }
    }

    companion object {
        @Suppress("LocalVariableName")
        fun fromRaw(
            raw: RawAssignment,
            vars: DistributedBasicVariables
        ): DistributedBasicAssignment = with(vars) {
            DistributedBasicAssignment(
                M = M,
                modularScenarioTree = modularScenarioTree,
                modularC = modularC,
                modularK = modularK,
                modularV = modularV,
                modularE = modularE,
                modularO = modularO,
                modularX = modularX,
                modularZ = modularZ,
                modularU = modularU,
                modularBasicAssignment = modularBasicVariables.map { BasicAssignment.fromRaw(raw, it) },
                stateUsed = stateUsed.convert(raw)
            )
        }
    }
}

fun DistributedBasicAssignment.toAutomaton(): DistributedAutomaton {
    val modules: MultiArray<Automaton> = modularBasicAssignment.mapIndexed { (m), a ->
        a.toAutomaton(stateUsed = { c -> stateUsed[m, c] })
    }
    return DistributedAutomaton(modules)
}
