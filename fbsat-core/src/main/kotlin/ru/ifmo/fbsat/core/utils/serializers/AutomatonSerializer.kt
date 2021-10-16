package ru.ifmo.fbsat.core.utils.serializers

import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import ru.ifmo.fbsat.core.automaton.Algorithm
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.AutomatonStats
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.guard.Guard
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.utils.toBinaryString

object AutomatonSerializer :
    KSerializer<Automaton> by SerializerViaSurrogate(AutomatonSurrogate)

@Serializable
@SerialName("Automaton")
private class AutomatonSurrogate private constructor(
    val stats: AutomatonStats,
    val states: List<State>,
    val prettyString: String,
) : Surrogate<Automaton> {
    @Serializable
    data class State(
        val id: Int,
        val outputEvent: OutputEvent?,
        val algorithm: Algorithm,
        val transitions: List<Transition>,
    )

    @Serializable
    data class Transition(
        val source: Int,
        val destination: Int,
        val inputEvent: InputEvent?,
        val guard: Guard,
    )

    override fun toOriginal(): Automaton = toAutomaton()

    fun toAutomaton(): Automaton {
        val automaton = Automaton(
            inputEvents = stats.inputEvents,
            outputEvents = stats.outputEvents,
            inputNames = stats.inputNames,
            outputNames = stats.outputNames,
            initialOutputValues = stats.initialOutputValues
        )
        for (state in states) {
            automaton.addState(
                id = state.id,
                outputEvent = state.outputEvent,
                algorithm = state.algorithm
            )
        }
        for (state in states) {
            for (transition in state.transitions) {
                automaton.addTransition(
                    sourceId = transition.source,
                    destinationId = transition.destination,
                    inputEvent = transition.inputEvent,
                    guard = transition.guard
                )
            }
        }
        return automaton
    }

    companion object Factory : Surrogate.Factory<Automaton, AutomatonSurrogate> {
        override val surrogateSerializer: KSerializer<AutomatonSurrogate> = serializer()

        override fun from(original: Automaton): AutomatonSurrogate {
            val states = original.states.map { s ->
                State(
                    id = s.id,
                    outputEvent = s.outputEvent,
                    algorithm = s.algorithm,
                    transitions = s.transitions.map { t ->
                        Transition(
                            source = t.source.id,
                            destination = t.destination.id,
                            inputEvent = t.inputEvent,
                            guard = t.guard
                        )
                    }
                )
            }
            val transitions = states.flatMap { it.transitions }
            val hash = run {
                val codeOutputEvents =
                    states.map {
                        if (it.outputEvent != null) original.outputEvents.indexOf(it.outputEvent) + 1 else 0
                    }
                val codeAlgorithms =
                    states.map {
                        with(it.algorithm as BinaryAlgorithm) {
                            algorithm0.toBinaryString().toInt(2) + algorithm1.toBinaryString().toInt(2)
                        }
                    }
                val codeTransitionDestination =
                    transitions.map {
                        it.destination
                    }
                val codeTransitionEvents =
                    transitions.map {
                        if (it.inputEvent != null) original.inputEvents.indexOf(it.inputEvent) + 1 else 0
                    }
                val codeTransitionGuards =
                    transitions.flatMap {
                        it.guard.truthTableString(original.inputNames)
                            .windowed(8, step = 8, partialWindows = true)
                            .map { s -> s.toInt(2) }
                    }
                (codeOutputEvents +
                    codeAlgorithms +
                    codeTransitionDestination +
                    codeTransitionEvents +
                    codeTransitionGuards)
                    .fold(0) { acc, i ->
                        (31 * acc + i).rem(1_000_000)
                    }
            }
            val stats = AutomatonStats(
                hash = hash,
                C = states.size,
                K = states.maxOfOrNull { it.transitions.size } ?: 0,
                P = transitions.maxOfOrNull { it.guard.size } ?: 0,
                T = transitions.size,
                N = transitions.sumOf { it.guard.size },
                I = original.inputEvents.size,
                O = original.outputEvents.size,
                X = original.inputNames.size,
                Z = original.outputNames.size,
                inputEvents = original.inputEvents,
                outputEvents = original.outputEvents,
                inputNames = original.inputNames,
                outputNames = original.outputNames,
                initialOutputValues = original.initialOutputValues
            )
            return AutomatonSurrogate(
                stats = stats,
                states = states,
                prettyString = original.toSimpleString()
            )
        }
    }
}
