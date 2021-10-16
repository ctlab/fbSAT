package ru.ifmo.fbsat.core.scenario

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.project

// Compound Event

sealed class CompoundEvent<E> : GenericEvent, CompoundImpl<E>()
    where E : Event?

class CompoundInputEvent(
    override val modular: MultiArray<InputEvent?>,
) : CompoundEvent<InputEvent?>(), GenericInputEvent

class CompoundOutputEvent(
    override val modular: MultiArray<OutputEvent?>,
) : CompoundEvent<OutputEvent?>(), GenericOutputEvent

// Compound Values

sealed class CompoundValues<V> : GenericValues, CompoundImpl<V>()
    where V : Values

class CompoundInputValues(
    override val modular: MultiArray<InputValues>,
) : CompoundValues<InputValues>(), GenericInputValues

class CompoundOutputValues(
    override val modular: MultiArray<OutputValues>,
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
    override val modular: MultiArray<InputAction>,
) : CompoundScenarioAction<CompoundInputEvent, CompoundInputValues, InputAction>(),
    GenericScenarioInputAction<CompoundInputEvent, CompoundInputValues> {

    constructor(M: Int, event: CompoundInputEvent, values: CompoundInputValues) :
        this(
            M = M,
            event = event,
            values = values,
            modular = MultiArray.new(M) { (m) ->
                InputAction(event.project(m), values.project(m))
            }
        )

    constructor(modular: MultiArray<InputAction>) :
        this(
            M = modular.shape.single(),
            event = CompoundInputEvent(modular.map { it.event }),
            values = CompoundInputValues(modular.map { it.values }),
            modular = modular
        )
}

class CompoundOutputAction private constructor(
    override val M: Int,
    override val event: CompoundOutputEvent,
    override val values: CompoundOutputValues,
    override val modular: MultiArray<OutputAction>,
) : CompoundScenarioAction<CompoundOutputEvent, CompoundOutputValues, OutputAction>(),
    GenericScenarioOutputAction<CompoundOutputEvent, CompoundOutputValues> {

    constructor(M: Int, event: CompoundOutputEvent, values: CompoundOutputValues) :
        this(
            M = M,
            event = event,
            values = values,
            modular = MultiArray.new(M) { (m) ->
                OutputAction(event.project(m), values.project(m))
            }
        )

    constructor(modular: MultiArray<OutputAction>) :
        this(
            M = modular.shape.single(),
            event = CompoundOutputEvent(modular.map { it.event }),
            values = CompoundOutputValues(modular.map { it.values }),
            modular = modular
        )
}
