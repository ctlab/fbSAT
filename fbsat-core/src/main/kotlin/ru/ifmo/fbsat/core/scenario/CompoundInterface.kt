package ru.ifmo.fbsat.core.scenario

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.project
import ru.ifmo.fbsat.core.utils.toImmutable

// Compound Event

sealed class CompoundEvent<E> : GenericEvent, CompoundImpl<E>()
    where E : Event?

class CompoundInputEvent(
    override val modular: ImmutableMultiArray<InputEvent?>,
) : CompoundEvent<InputEvent?>(), GenericInputEvent

class CompoundOutputEvent(
    override val modular: ImmutableMultiArray<OutputEvent?>,
) : CompoundEvent<OutputEvent?>(), GenericOutputEvent

// Compound Values

sealed class CompoundValues<V> : GenericValues, CompoundImpl<V>()
    where V : Values

class CompoundInputValues(
    override val modular: ImmutableMultiArray<InputValues>,
) : CompoundValues<InputValues>(), GenericInputValues

class CompoundOutputValues(
    override val modular: ImmutableMultiArray<OutputValues>,
) : CompoundValues<OutputValues>(), GenericOutputValues

// Compound Action

sealed class CompoundScenarioAction<E, V, A> : GenericScenarioAction<E, V>, CompoundImpl<A>()
    where E : CompoundEvent<*>,
          V : CompoundValues<*>,
          A : ScenarioAction<*, *>

class CompoundInputAction private constructor(
    override val M: Int,
    override val event: CompoundInputEvent,
    override val values: CompoundInputValues,
    override val modular: ImmutableMultiArray<InputAction>,
) : CompoundScenarioAction<CompoundInputEvent, CompoundInputValues, InputAction>(),
    GenericScenarioInputAction<CompoundInputEvent, CompoundInputValues> {

    constructor(M: Int, event: CompoundInputEvent, values: CompoundInputValues) :
        this(
            M = M,
            event = event,
            values = values,
            modular = MultiArray.create(M) { (m) ->
                InputAction(event.project(m), values.project(m))
            }.toImmutable()
        )

    constructor(modular: MultiArray<InputAction>) :
        this(
            M = modular.shape.single(),
            event = CompoundInputEvent(modular.map { it.event }.toImmutable()),
            values = CompoundInputValues(modular.map { it.values }.toImmutable()),
            modular = modular.toImmutable()
        )
}

class CompoundOutputAction private constructor(
    override val M: Int,
    override val event: CompoundOutputEvent,
    override val values: CompoundOutputValues,
    override val modular: ImmutableMultiArray<OutputAction>,
) : CompoundScenarioAction<CompoundOutputEvent, CompoundOutputValues, OutputAction>(),
    GenericScenarioOutputAction<CompoundOutputEvent, CompoundOutputValues> {

    constructor(M: Int, event: CompoundOutputEvent, values: CompoundOutputValues) :
        this(
            M = M,
            event = event,
            values = values,
            modular = MultiArray.create(M) { (m) ->
                OutputAction(event.project(m), values.project(m))
            }.toImmutable()
        )

    constructor(modular: MultiArray<OutputAction>) :
        this(
            M = modular.shape.single(),
            event = CompoundOutputEvent(modular.map { it.event }.toImmutable()),
            values = CompoundOutputValues(modular.map { it.values }.toImmutable()),
            modular = modular.toImmutable()
        )
}
