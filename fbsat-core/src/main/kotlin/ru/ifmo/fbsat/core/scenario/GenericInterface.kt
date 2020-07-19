package ru.ifmo.fbsat.core.scenario

interface GenericEvent
interface GenericValues

@Deprecated("This is meaningless")
interface GenericInputEvent : GenericEvent

@Deprecated("This is meaningless")
interface GenericOutputEvent : GenericEvent

@Deprecated("This is meaningless")
interface GenericInputValues : GenericValues

@Deprecated("This is meaningless")
interface GenericOutputValues : GenericValues

@Deprecated("This is meaningless")
interface GenericScenarioAction<E, V>
    where E : GenericEvent,
          V : GenericValues

@Deprecated("This is meaningless")
interface GenericScenarioInputAction<E, V> : GenericScenarioAction<E, V>
    where E : GenericInputEvent,
          V : GenericInputValues

@Deprecated("This is meaningless")
interface GenericScenarioOutputAction<E, V> : GenericScenarioAction<E, V>
    where E : GenericOutputEvent,
          V : GenericOutputValues
