package ru.ifmo.fbsat.core.scenario.positive

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.CompoundScenario
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.scenario.Scenario
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.toImmutable
import ru.ifmo.fbsat.core.utils.toMultiArray

class PositiveCompoundScenario private constructor(
    override val M: Int,
    override val elements: List<CompoundScenarioElement>,
    override val modular: ImmutableMultiArray<PositiveScenario>
) : CompoundScenario, CompoundImpl<Scenario>() {
    // TODO: constructor(modularPositiveScenario: MultiArray<PositiveScenario>)

    constructor(M: Int, elements: List<CompoundScenarioElement>) : this(
        M = M,
        elements = elements,
        modular = MultiArray.create(M) { (m) ->
            PositiveScenario(elements.map { it.modular.toMultiArray()[m] })
        }.toImmutable()
    )

    // Note: `constructor(modular: MultiArray<PositiveScenario>)` is prohibited
    // because modular scenarios might be of different sizes and couldn't be zip'ped
}
