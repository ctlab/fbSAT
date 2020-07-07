package ru.ifmo.fbsat.core.task

import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.distributed.DistributedBasicVariables
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.ArbitraryModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.ConsecutiveModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.basic.parallel.ParallelModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.ConsecutiveModularExtendedVariables
import ru.ifmo.fbsat.core.task.modular.extended.parallel.ParallelModularExtendedVariables
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.complete.CompleteVariables
import ru.ifmo.fbsat.core.task.single.extended.ExtendedVariables
import ru.ifmo.fbsat.core.task.single.extforest.ExtForestVariables

// TODO: Rename to 'ContextVar'
enum class VARS {
    BASIC,
    EXTENDED,
    COMPLETE,
    PARALLEL_MODULAR_BASIC,
    CONSECUTIVE_MODULAR_BASIC,
    ARBITRARY_MODULAR_BASIC,
    PARALLEL_MODULAR_EXTENDED,
    CONSECUTIVE_MODULAR_EXTENDED,
    DISTRIBUTED_BASIC,
    EXTFOREST;

    override fun toString(): String {
        return "${name}_VARS"
    }
}

// operator fun <K : Any, T> SolverContext.get(key: K): T = get(key.toString())
//
// operator fun <K : Any> SolverContext.set(key: K, value: Any) {
//     set(key.toString(), value)
// }

// operator fun <T> SolverContext.get(key: Vars): T = get(key.toString())
//
// operator fun SolverContext.set(key: Vars, value: Any) {
//     set(key.toString(), value)
// }

val Solver.basicVars: BasicVariables get() = context[VARS.BASIC]
val Solver.extendedVars: ExtendedVariables get() = context[VARS.EXTENDED]
val Solver.completeVars: CompleteVariables get() = context[VARS.COMPLETE]
val Solver.parallelModularBasicVars: ParallelModularBasicVariables get() = context[VARS.PARALLEL_MODULAR_BASIC]
val Solver.consecutiveModularBasicVars: ConsecutiveModularBasicVariables get() = context[VARS.CONSECUTIVE_MODULAR_BASIC]
val Solver.arbitraryModularBasicVars: ArbitraryModularBasicVariables get() = context[VARS.ARBITRARY_MODULAR_BASIC]
val Solver.parallelModularExtendedVars: ParallelModularExtendedVariables get() = context[VARS.PARALLEL_MODULAR_EXTENDED]
val Solver.consecutiveModularExtendedVars: ConsecutiveModularExtendedVariables get() = context[VARS.CONSECUTIVE_MODULAR_EXTENDED]
val Solver.distributedBasicVars: DistributedBasicVariables get() = context[VARS.DISTRIBUTED_BASIC]
val Solver.extForestVars: ExtForestVariables get() = context[VARS.EXTFOREST]
