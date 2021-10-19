package ru.ifmo.fbsat.core.scenario

interface GenericScenario<out E : GenericScenario.Element<*, *>> {
    val elements: List<E>

    interface Element<out In, out Out>
        where In : GenericScenarioInputAction<*, *>,
              Out : GenericScenarioOutputAction<*, *> {
        var nodeId: Int?
        val inputAction: In
        val outputAction: Out
    }
}

val <E, In> GenericScenario<E>.inputActions: List<In>
    where E : GenericScenario.Element<In, *>
    get() = elements.map { it.inputAction }
val <E, In> GenericScenario<E>.inputActionsSeq: Sequence<In>
    where E : GenericScenario.Element<In, *>
    get() = elements.asSequence().map { it.inputAction }

val <E, Out> GenericScenario<E>.outputActions: List<Out>
    where E : GenericScenario.Element<*, Out>
    get() = elements.map { it.outputAction }
val <E, Out> GenericScenario<E>.outputActionsSeq: Sequence<Out>
    where E : GenericScenario.Element<*, Out>
    get() = elements.asSequence().map { it.outputAction }

val <In, E>  GenericScenario.Element<In, *>.inputEvent: E?
    where In : GenericScenarioInputAction<E, *>,
          E : GenericInputEvent
    get() = inputAction.event
val <In, V>  GenericScenario.Element<In, *>.inputValues: V
    where In : GenericScenarioInputAction<*, V>,
          V : GenericInputValues
    get() = inputAction.values

val <Out, E>  GenericScenario.Element<*, Out>.outputEvent: E?
    where Out : GenericScenarioOutputAction<E, *>,
          E : GenericOutputEvent
    get() = outputAction.event
val <Out, V>  GenericScenario.Element<*, Out>.outputValues: V
    where Out : GenericScenarioOutputAction<*, V>,
          V : GenericOutputValues
    get() = outputAction.values
