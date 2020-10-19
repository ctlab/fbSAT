package ru.ifmo.fbsat.core.task

import ru.ifmo.fbsat.core.solver.SolverContext
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
