package ru.ifmo.fbsat.core.task.distributed.complete

import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.iffAnd
import ru.ifmo.fbsat.core.solver.iffImply
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.distributedCompleteVars
import ru.ifmo.fbsat.core.task.distributedExtendedVars
import ru.ifmo.fbsat.core.task.single.complete.updateNegativeReduction
import ru.ifmo.fbsat.core.utils.log

data class DistributedCompleteTask(
    val numberOfModules: Int, // M
    val negativeCompoundScenarioTree: NegativeCompoundScenarioTree? = null // empty if null
) : Task() {
    override fun Solver.declare_() {
        val modularTree = context.distributedExtendedVars.modularScenarioTree
        val negTree = negativeCompoundScenarioTree ?: NegativeCompoundScenarioTree(
            numberOfModules,
            modularInputEvents = modularTree.map { it.inputEvents },
            modularOutputEvents = modularTree.map { it.outputEvents },
            modularInputNames = modularTree.map { it.inputNames },
            modularOutputNames = modularTree.map { it.outputNames }
        )

        /* Variables */
        val vars = declareDistributedCompleteVariables(
            extendedVars = context.distributedExtendedVars,
            negativeCompoundScenarioTree = negTree
        ).also {
            context.distributedCompleteVars = it
        }

        /* Initial negative constraints */
        updateDistributedNegativeReduction(vars)
    }
}

fun Solver.updateDistributedNegativeReduction(vars: DistributedCompleteVariables): Unit = with(vars) {
    for (m in 1..M) {
        comment("Update negative reduction: for module m = $m")
        updateNegativeReduction(modularCompleteVariables[m], isForbidLoops = false)
    }

    comment("Forbid loops (compound)")
    val compoundNegV = negativeCompoundScenarioTree.size
    for (v in 1..compoundNegV)
        for (l in negativeCompoundScenarioTree.loopBacks(v))
            if (forbiddenLoops.add(v to l)) {
                log.debug { "Forbidding loop from v = $v to l = $l..." }
                // OR_{m in 1..M}( aux1_m )
                clause {
                    for (m in 1..M) with(modularCompleteVariables[m]) {
                        // aux1_m <=> AND_{c in 1..C}( aux2_c )
                        val aux1 = newLiteral()
                        iffAnd(aux1, sequence {
                            for (c in 1..modularC[m]) {
                                // aux2_c <=> ((negMapping[v] = c) => (negMapping[l] != c))
                                val aux2 = newLiteral()
                                iffImply(aux2, negMapping[v] eq c, negMapping[l] neq c)
                                yield(aux2)
                            }
                        })
                        yield(aux1)
                    }
                }
            }
}
