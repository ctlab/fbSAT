package ru.ifmo.fbsat.core.scenario.positive

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.CompoundScenario
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.stringify
import ru.ifmo.fbsat.core.utils.toImmutable
import ru.ifmo.fbsat.core.utils.toMultiArray

class PositiveCompoundScenario private constructor(
    override val elements: List<CompoundScenarioElement>,
    override val modular: ImmutableMultiArray<PositiveScenario>
) : CompoundScenario {
    // TODO: constructor(modularPositiveScenario: MultiArray<PositiveScenario>)

    constructor(M: Int, elements: List<CompoundScenarioElement>) : this(
        elements = elements,
        modular = MultiArray.create(M) { (m) ->
            PositiveScenario(elements.map { it.modular.toMultiArray()[m] })
        }
    )

    @Deprecated("This constructor will be removed after the ImmutableMultiArray is stabilized")
    constructor(elements: List<CompoundScenarioElement>, modular: MultiArray<PositiveScenario>) :
        this(elements, modular.toImmutable())

    override fun toString(): String = stringify()
}
