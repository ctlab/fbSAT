package ru.ifmo.fbsat.core.scenario.negative

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.CompoundScenario
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.scenario.Scenario
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.toImmutable
import ru.ifmo.fbsat.core.utils.toMultiArray

class NegativeCompoundScenario private constructor(
    override val M: Int,
    override val elements: List<CompoundScenarioElement>,
    override val modular: ImmutableMultiArray<NegativeScenario>
) : CompoundScenario, CompoundImpl<Scenario>() {
    // TODO: constructor(modularNegativeScenario: MultiArray<NegativeScenario>)

    constructor(M: Int, elements: List<CompoundScenarioElement>, loopPosition: Int?) : this(
        M = M,
        elements = elements,
        modular = MultiArray.create(M) { (m) ->
            NegativeScenario(elements.map { it.modular.toMultiArray()[m] }, loopPosition)
        }.toImmutable()
    )
}
