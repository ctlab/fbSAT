package ru.ifmo.fbsat.core.task.distributed.complete

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.SolverContext
import ru.ifmo.fbsat.core.solver.switchContext
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.single.complete.updateNegativeReduction

data class DistributedCompleteTask(
    val numberOfModules: Int, // M
    val negativeCompoundScenarioTree: NegativeCompoundScenarioTree? = null // empty if null
) : Task() {
    override fun Solver.declare_() {
        // FIXME
        val modularTree: MultiArray<PositiveScenarioTree> by context
        val negTree = negativeCompoundScenarioTree ?: NegativeCompoundScenarioTree(
            numberOfModules,
            modularInputEvents = modularTree.map { it.inputEvents },
            modularOutputEvents = modularTree.map { it.outputEvents },
            modularInputNames = modularTree.map { it.inputNames },
            modularOutputNames = modularTree.map { it.outputNames }
        )

        /* Variables */
        comment("$name: Variables")
        declareDistributedCompleteVariables(
            negativeCompoundScenarioTree = negTree
        )

        /* Initial negative constraints */
        comment("$name: Initial negative constraints")
        updateDistributedNegativeReduction()
    }
}

fun Solver.updateDistributedNegativeReduction() {
    val M: Int by context
    val modularContext: MultiArray<SolverContext> by context

    for (m in 1..M) switchContext(modularContext[m]) {
        comment("Update negative reduction: for module m = $m")
        updateNegativeReduction(isForbidLoops = false)
    }

    comment("Forbid loops (compound)")
    TODO()
    // val compoundNegV = negativeCompoundScenarioTree.size
    // for (v in 1..compoundNegV)
    //     for (l in negativeCompoundScenarioTree.loopBacks(v))
    //         if (forbiddenLoops.add(v to l)) {
    //             // log.debug { "Forbidding loop from v = $v to l = $l..." }
    //             // OR_{m in 1..M}( aux1_m )
    //             clause {
    //                 for (m in 1..M) with(modularCompleteVariables[m]) {
    //                     // aux1_m <=> AND_{c in 1..C}( aux2_c )
    //                     val aux1 = newLiteral()
    //                     iffAnd(aux1, sequence {
    //                         for (c in 1..modularC[m]) {
    //                             // aux2_c <=> ((negMapping[v] = c) => (negMapping[l] != c))
    //                             val aux2 = newLiteral()
    //                             iffImply(aux2, negMapping[v] eq c, negMapping[l] neq c)
    //                             yield(aux2)
    //                         }
    //                     })
    //                     yield(aux1)
    //                 }
    //             }
    //         }
}
