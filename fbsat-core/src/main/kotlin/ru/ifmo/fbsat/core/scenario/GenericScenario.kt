package ru.ifmo.fbsat.core.scenario

interface GenericScenario<out E>
    where E : GenericScenario.Element<*, *> {

    val elements: List<E>

    interface Element<In, Out>
        where In : GenericScenarioInputAction<*, *>,
              Out : GenericScenarioOutputAction<*, *> {
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
