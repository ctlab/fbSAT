package ru.ifmo.fbsat.core.automaton

import kotlinx.serialization.Serializable
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues

@Serializable
data class AutomatonStats(
    val hash: Int,
    val C: Int,
    val K: Int,
    val P: Int,
    val T: Int,
    val N: Int,
    val I: Int,
    val O: Int,
    val X: Int,
    val Z: Int,
    val inputEvents: List<InputEvent>,
    val outputEvents: List<OutputEvent>,
    val inputNames: List<String>,
    val outputNames: List<String>,
    val initialOutputValues: OutputValues,
) {
    constructor(automaton: Automaton) : this(
        hash = automaton.calculateHashCode(),
        C = automaton.numberOfStates,
        K = automaton.maxOutgoingTransitions,
        P = automaton.maxGuardSize,
        T = automaton.numberOfTransitions,
        N = automaton.totalGuardsSize,
        I = automaton.inputEvents.size,
        O = automaton.outputEvents.size,
        X = automaton.inputNames.size,
        Z = automaton.outputNames.size,
        inputEvents = automaton.inputEvents,
        outputEvents = automaton.outputEvents,
        inputNames = automaton.inputNames,
        outputNames = automaton.outputNames,
        initialOutputValues = automaton.initialOutputValues,
    )
}

fun Automaton.stats(): AutomatonStats = AutomatonStats(this)
