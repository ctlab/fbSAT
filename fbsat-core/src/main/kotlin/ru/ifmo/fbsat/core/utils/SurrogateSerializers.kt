@file:Suppress("unused")

package ru.ifmo.fbsat.core.utils

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encodeToString
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.modules.SerializersModule
import kotlinx.serialization.modules.polymorphic
import kotlinx.serialization.modules.subclass
import ru.ifmo.fbsat.core.automaton.Algorithm
import ru.ifmo.fbsat.core.automaton.Automaton
import ru.ifmo.fbsat.core.automaton.AutomatonStats
import ru.ifmo.fbsat.core.automaton.BinaryAlgorithm
import ru.ifmo.fbsat.core.automaton.BooleanExpressionGuard
import ru.ifmo.fbsat.core.automaton.Guard
import ru.ifmo.fbsat.core.automaton.ParseTreeGuard
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.OutputEvent
import kotlin.random.Random

@Suppress("PublicApiImplicitType")
val surrogateModule = SerializersModule {
    polymorphic(Surrogate::class) {
        subclass(AutomatonSurrogate::class)
        subclass(BinaryAlgorithmSurrogate::class)
        subclass(ParseTreeGuardSurrogate::class)
    }
}

interface Surrogate<out T> {
    fun toOriginal(): T

    interface Factory<T, ST : Surrogate<T>> {
        val surrogateSerializer: KSerializer<ST> // Note: just use `= serializer()` as implementation

        fun from(original: T): ST
    }
}

abstract class AbstractSerializerViaSurrogate<T, out ST : Surrogate<T>>(
    // private val surrogateSerializer: KSerializer<ST>,
    private val surrogateFactory: Surrogate.Factory<T, ST>,
) : KSerializer<T> {
    private val surrogateSerializer: KSerializer<ST> = surrogateFactory.surrogateSerializer

    final override val descriptor: SerialDescriptor = surrogateSerializer.descriptor

    final override fun serialize(encoder: Encoder, value: T) {
        val surrogate = surrogateFactory.from(value)
        encoder.encodeSerializableValue(surrogateSerializer, surrogate)
    }

    final override fun deserialize(decoder: Decoder): T {
        val surrogate: Surrogate<T> = decoder.decodeSerializableValue(surrogateSerializer)
        return surrogate.toOriginal()
    }
}

inline fun <T, reified ST : Surrogate<T>> serializerViaSurrogate(
    surrogateFactory: Surrogate.Factory<T, ST>,
): KSerializer<T> =
    object : AbstractSerializerViaSurrogate<T, ST>(
        // serializer(),
        surrogateFactory
    ) {}

//region Automaton

object AutomatonSerializer :
    KSerializer<Automaton> by serializerViaSurrogate(AutomatonSurrogate)

@Serializable
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
            outputNames = stats.outputNames
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
                outputNames = original.outputNames
            )
            return AutomatonSurrogate(
                stats = stats,
                states = states,
                prettyString = original.toSimpleString()
            )
        }
    }
}

//endregion

//region ParseTreeGuard

object ParseTreeGuardSerializer :
    KSerializer<ParseTreeGuard> by serializerViaSurrogate(ParseTreeGuardSurrogate)

@Serializable
private class ParseTreeGuardSurrogate private constructor(
    val expr: String,
) : Surrogate<ParseTreeGuard> {
    override fun toOriginal(): ParseTreeGuard = toParseTreeGuard()

    fun toParseTreeGuard(): ParseTreeGuard {
        TODO("Create ParseTreeGuard from string `expr`")
    }

    companion object Factory : Surrogate.Factory<ParseTreeGuard, ParseTreeGuardSurrogate> {
        override val surrogateSerializer: KSerializer<ParseTreeGuardSurrogate> = serializer()

        override fun from(original: ParseTreeGuard): ParseTreeGuardSurrogate {
            val expr = original.toSimpleString()
            return ParseTreeGuardSurrogate(
                expr = expr
            )
        }
    }
}

//endregion

//region BinaryAlgorithm

// Look ma, no body!
object BinaryAlgorithmSerializer :
    KSerializer<BinaryAlgorithm> by serializerViaSurrogate(BinaryAlgorithmSurrogate)

@Serializable
private class BinaryAlgorithmSurrogate private constructor(
    val algorithm0: String,
    val algorithm1: String,
) : Surrogate<BinaryAlgorithm> {
    fun toBinaryAlgorithm(): BinaryAlgorithm = BinaryAlgorithm(algorithm0, algorithm1)

    override fun toOriginal(): BinaryAlgorithm = toBinaryAlgorithm()

    companion object Factory : Surrogate.Factory<BinaryAlgorithm, BinaryAlgorithmSurrogate> {
        override val surrogateSerializer: KSerializer<BinaryAlgorithmSurrogate> = serializer()

        override fun from(original: BinaryAlgorithm): BinaryAlgorithmSurrogate {
            val a0 = original.algorithm0.toBinaryString()
            val a1 = original.algorithm1.toBinaryString()
            return BinaryAlgorithmSurrogate(a0, a1)
        }
    }
}

//endregion

//region BooleanExpressionGuard

object BooleanExpressionGuardSerializer :
    KSerializer<BooleanExpressionGuard> by serializerViaSurrogate(BooleanExpressionGuardSurrogate)

@Serializable
private class BooleanExpressionGuardSurrogate private constructor(
    val expr: String,
    val ast: BooleanExpression,
) : Surrogate<BooleanExpressionGuard> {
    override fun toOriginal(): BooleanExpressionGuard = toBooleanExpressionGuard()

    fun toBooleanExpressionGuard(): BooleanExpressionGuard = BooleanExpressionGuard(ast)

    companion object Factory : Surrogate.Factory<BooleanExpressionGuard, BooleanExpressionGuardSurrogate> {
        override val surrogateSerializer: KSerializer<BooleanExpressionGuardSurrogate> = serializer()

        override fun from(original: BooleanExpressionGuard): BooleanExpressionGuardSurrogate {
            val expr = original.toSimpleString()
            val ast = original.expr
            return BooleanExpressionGuardSurrogate(expr, ast)
        }
    }
}

//endregion

fun main() {
    val X = 3
    val inputNames = (1..X).map { "x$it" }

    val guard = BooleanExpressionGuard(randomBooleanExpression(5, inputNames, Random(42)))
    println("guard = ${guard.toSimpleString()}")

    val s = fbsatJson.encodeToString(guard)
    println("guard -> json = $s")

    val guard2: BooleanExpressionGuard = fbsatJson.decodeFromString(s)
    println("json -> ${guard2.toSimpleString()}")
}
