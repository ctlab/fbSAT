package ru.ifmo.fbsat.core.scenario

import ru.ifmo.fbsat.core.utils.Compound
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.stringify

// Compound Event

sealed class CompoundEvent : GenericEvent, Compound<Event> {
    override fun toString(): String = stringify()
}

data class CompoundInputEvent(
    override val modular: ImmutableMultiArray<Event>
) : CompoundEvent(),
    GenericInputEvent

data class CompoundOutputEvent(
    override val modular: ImmutableMultiArray<Event>
) : CompoundEvent(),
    GenericOutputEvent

// Compound Values

sealed class CompoundValues : GenericValues, Compound<Values> {
    override fun toString(): String = stringify()
}

data class CompoundInputValues(
    override val modular: ImmutableMultiArray<Values>
) : CompoundValues(),
    GenericInputValues

data class CompoundOutputValues(
    override val modular: ImmutableMultiArray<Values>
) : CompoundValues(),
    GenericOutputValues

// Compound Action

sealed class CompoundScenarioAction<E, V, A> : GenericScenarioAction<E, V>, Compound<A>
    where E : CompoundEvent,
          V : CompoundValues,
          A : ScenarioAction<*, *> {
    override fun toString(): String = stringify()
}

data class CompoundInputAction(
    override val modular: ImmutableMultiArray<InputAction>
) : CompoundScenarioAction<CompoundInputEvent, CompoundInputValues, InputAction>(),
    GenericScenarioInputAction<CompoundInputEvent, CompoundInputValues>

data class CompoundOutputAction(
    override val modular: ImmutableMultiArray<OutputAction>
) : CompoundScenarioAction<CompoundOutputEvent, CompoundOutputValues, OutputAction>(),
    GenericScenarioOutputAction<CompoundOutputEvent, CompoundOutputValues>
