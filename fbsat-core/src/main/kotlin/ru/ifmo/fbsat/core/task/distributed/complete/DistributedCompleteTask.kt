package ru.ifmo.fbsat.core.task.distributed.complete

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import com.github.lipen.satlib.core.IntVarArray
import com.github.lipen.satlib.core.eq
import com.github.lipen.satlib.core.neq
import com.github.lipen.satlib.op.iffAnd
import com.github.lipen.satlib.op.iffImply
import com.github.lipen.satlib.solver.Solver
import ru.ifmo.fbsat.core.scenario.negative.NegativeCompoundScenarioTree
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenarioTree
import ru.ifmo.fbsat.core.solver.clause
import ru.ifmo.fbsat.core.solver.forEachModularContext
import ru.ifmo.fbsat.core.task.Task
import ru.ifmo.fbsat.core.task.single.complete.updateNegativeReduction

data class DistributedCompleteTask(
    val numberOfModules: Int, // M
    val negativeCompoundScenarioTree: NegativeCompoundScenarioTree? = null, // empty if null
) : Task() {
    override fun Solver.declare_() {
        // FIXME
        val modularTree: MultiArray<PositiveScenarioTree> = context["modularTree"]
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
    forEachModularContext { m ->
        comment("Update negative reduction: for module m = $m")
        updateNegativeReduction(isForbidLoops = false)
    }

    comment("Forbid loops (compound)")
    val negativeCompoundScenarioTree: NegativeCompoundScenarioTree = context["negativeCompoundScenarioTree"]
    val forbiddenLoops: MutableSet<Pair<Int, Int>> = context["forbiddenLoops"]
    val compoundNegV = negativeCompoundScenarioTree.size
    for (v in 1..compoundNegV)
        for (l in negativeCompoundScenarioTree.loopBacks(v))
            if (forbiddenLoops.add(v to l)) {
                // log.debug { "Forbidding loop from v = $v to l = $l..." }
                // OR_{m in 1..M}( aux1_m )
                clause {
                    forEachModularContext {
                        val C: Int = context["C"]
                        val negMapping: IntVarArray = context["negMapping"]

                        // aux1_m <=> AND_{c in 1..C}( aux2_c )
                        val aux1 = newLiteral()
                        iffAnd(aux1) {
                            for (c in 1..C) {
                                // aux2_c <=> ((negMapping[v] = c) => (negMapping[l] != c))
                                val aux2 = newLiteral()
                                iffImply(aux2, negMapping[v] eq c, negMapping[l] neq c)
                                yield(aux2)
                            }
                        }
                        yield(aux1)
                    }
                }
            }
}
