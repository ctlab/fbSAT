package ru.ifmo.fbsat.core.task

import ru.ifmo.fbsat.core.solver.SolverContext
import ru.ifmo.fbsat.core.task.distributed.basic.DistributedBasicVariables
import ru.ifmo.fbsat.core.task.distributed.extended.DistributedExtendedVariables
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.ArbitraryModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.ConsecutiveModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.basic.parallel.ParallelModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.ConsecutiveModularExtendedVariables
import ru.ifmo.fbsat.core.task.modular.extended.parallel.ParallelModularExtendedVariables
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.complete.CompleteVariables
import ru.ifmo.fbsat.core.task.single.extended.ExtendedVariables
import ru.ifmo.fbsat.core.task.single.extforest.ExtForestVariables
import kotlin.properties.ReadWriteProperty
import kotlin.reflect.KProperty

private inline fun <reified T : Any> register(): ReadWriteProperty<SolverContext, T> =
    object : ReadWriteProperty<SolverContext, T> {
        private fun key(property: KProperty<*>): String {
            return "_${T::class.java.simpleName}_${property.name}"
        }

        override fun getValue(thisRef: SolverContext, property: KProperty<*>): T {
            return thisRef[key(property)]
        }

        override fun setValue(thisRef: SolverContext, property: KProperty<*>, value: T) {
            thisRef[key(property)] = value
        }
    }

var SolverContext.basicVars: BasicVariables by register()
var SolverContext.extendedVars: ExtendedVariables by register()
var SolverContext.completeVars: CompleteVariables by register()
var SolverContext.parallelModularBasicVars: ParallelModularBasicVariables by register()
var SolverContext.consecutiveModularBasicVars: ConsecutiveModularBasicVariables by register()
var SolverContext.arbitraryModularBasicVars: ArbitraryModularBasicVariables by register()
var SolverContext.parallelModularExtendedVars: ParallelModularExtendedVariables by register()
var SolverContext.consecutiveModularExtendedVars: ConsecutiveModularExtendedVariables by register()
var SolverContext.distributedBasicVars: DistributedBasicVariables by register()
var SolverContext.distributedExtendedVars: DistributedExtendedVariables by register()

// var SolverContext.distributedCompleteVars: DistributedCompleteVariables by register()
var SolverContext.extForestVars: ExtForestVariables by register()
