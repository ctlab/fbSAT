package ru.ifmo.fbsat.core.task

import ru.ifmo.fbsat.core.solver.Solver
import ru.ifmo.fbsat.core.task.modular.basic.arbitrary.ArbitraryModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.basic.consecutive.ConsecutiveModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.basic.parallel.ParallelModularBasicVariables
import ru.ifmo.fbsat.core.task.modular.extended.consecutive.ConsecutiveModularExtendedVariables
import ru.ifmo.fbsat.core.task.single.basic.BasicVariables
import ru.ifmo.fbsat.core.task.single.basic2.BasicVariables2
import ru.ifmo.fbsat.core.task.single.complete.CompleteVariables
import ru.ifmo.fbsat.core.task.single.extended.ExtendedVariables
import ru.ifmo.fbsat.core.task.single.extforest.ExtForestVariables

const val BASIC_VARS: String = "basicVariables"
const val EXTENDED_VARS: String = "extendedVariables"
const val COMPLETE_VARS: String = "completeVariables"
const val PARALLEL_MODULAR_BASIC_VARS: String = "parallelModularBasicVariables"
const val CONSECUTIVE_MODULAR_BASIC_VARS: String = "consecutiveModularBasicVariables"
const val ARBITRARY_MODULAR_BASIC_VARS: String = "arbitraryModularBasicVariables"
const val CONSECUTIVE_MODULAR_EXTENDED_VARS: String = "consecutiveModularExtendedVariables"
const val EXTFOREST_VARS: String = "extForestVariables"

val Solver.basicVars: BasicVariables get() = context[BASIC_VARS]
val Solver.basicVars2: BasicVariables2 get() = context[BASIC_VARS]
val Solver.extendedVars: ExtendedVariables get() = context[EXTENDED_VARS]
val Solver.completeVars: CompleteVariables get() = context[COMPLETE_VARS]
val Solver.parallelModularBasicVars: ParallelModularBasicVariables get() = context[PARALLEL_MODULAR_BASIC_VARS]
val Solver.consecutiveModularBasicVars: ConsecutiveModularBasicVariables get() = context[CONSECUTIVE_MODULAR_BASIC_VARS]
val Solver.arbitraryModularBasicVars: ArbitraryModularBasicVariables get() = context[ARBITRARY_MODULAR_BASIC_VARS]
val Solver.consecutiveModularExtendedVars: ConsecutiveModularExtendedVariables get() = context[CONSECUTIVE_MODULAR_EXTENDED_VARS]
val Solver.extForestVars: ExtForestVariables get() = context[EXTFOREST_VARS]
