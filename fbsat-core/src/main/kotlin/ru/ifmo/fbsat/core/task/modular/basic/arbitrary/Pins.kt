package ru.ifmo.fbsat.core.task.modular.basic.arbitrary

import com.github.lipen.multiarray.MultiArray

@Suppress("MemberVisibilityCanBePrivate", "unused")
internal class Pins(
    val M: Int,
    val X: Int,
    val Z: Int,
    val E: Int,
    val O: Int,
) {
    val modularInboundVarPins: MultiArray<List<Int>> =
        MultiArray.create(M) { (m) -> (1..X).map { (m - 1) * X + it } }
    val modularOutboundVarPins: MultiArray<List<Int>> =
        MultiArray.create(M) { (m) -> (1..Z).map { (m - 1) * Z + it } }
    val externalInboundVarPins: List<Int> =
        (1..Z).map { M * X + it }
    val externalOutboundVarPins: List<Int> =
        (1..X).map { M * Z + it }
    val allInboundVarPins: List<Int> =
        modularInboundVarPins.values.flatten() + externalInboundVarPins
    val allOutboundVarPins: List<Int> =
        modularOutboundVarPins.values.flatten() + externalOutboundVarPins

    val modularInboundEventPins: MultiArray<List<Int>> =
        MultiArray.create(M) { (m) -> (1..E).map { (m - 1) * E + it } }
    val modularOutboundEventPins: MultiArray<List<Int>> =
        MultiArray.create(M) { (m) -> (1..O).map { (m - 1) * O + it } }
    val externalInboundEventPins: List<Int> =
        (1..E).map { M * O + it }
    val externalOutboundEventPins: List<Int> =
        (1..O).map { M * E + it }
    val allInboundEventPins: List<Int> =
        modularInboundEventPins.values.flatten() + externalOutboundEventPins
    val allOutboundEventPins: List<Int> =
        modularOutboundEventPins.values.flatten() + externalInboundEventPins
}
