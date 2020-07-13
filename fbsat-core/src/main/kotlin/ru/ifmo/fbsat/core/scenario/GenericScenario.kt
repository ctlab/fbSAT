package ru.ifmo.fbsat.core.scenario

interface GenericScenario<out E, In, Out>
    where E : GenericScenario.Element<In, Out> {

    val elements: List<E>

    interface Element<In, Out> {
        val inputAction: In
        val outputAction: Out
    }
}

val <E, In> GenericScenario<E, In, *>.inputActions: List<In>
    where E : GenericScenario.Element<In, *>
    get() = elements.map { it.inputAction }
val <E, In> GenericScenario<E, In, *>.inputActionsSeq: Sequence<In>
    where E : GenericScenario.Element<In, *>
    get() = elements.asSequence().map { it.inputAction }

val <E, Out> GenericScenario<E, *, Out>.outputActions: List<Out>
    where E : GenericScenario.Element<*, Out>
    get() = elements.map { it.outputAction }
val <E, Out> GenericScenario<E, *, Out>.outputActionsSeq: Sequence<Out>
    where E : GenericScenario.Element<*, Out>
    get() = elements.asSequence().map { it.outputAction }
