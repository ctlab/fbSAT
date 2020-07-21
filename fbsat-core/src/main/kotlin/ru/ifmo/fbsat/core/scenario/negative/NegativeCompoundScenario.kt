package ru.ifmo.fbsat.core.scenario.negative

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.CompoundScenario
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.project
import ru.ifmo.fbsat.core.utils.toImmutable

class NegativeCompoundScenario private constructor(
    override val M: Int,
    override val elements: List<CompoundScenarioElement>,
    val loopPosition: Int?,
    override val modular: ImmutableMultiArray<NegativeScenario>
) : CompoundScenario<NegativeScenario>, CompoundImpl<NegativeScenario>() {
    // TODO: constructor(modularNegativeScenario: MultiArray<NegativeScenario>)

    constructor(M: Int, elements: List<CompoundScenarioElement>, loopPosition: Int?) : this(
        M = M,
        elements = elements,
        loopPosition = loopPosition,
        modular = MultiArray.create(M) { (m) ->
            NegativeScenario(elements.project(m), loopPosition)
        }.toImmutable()
    )
}
