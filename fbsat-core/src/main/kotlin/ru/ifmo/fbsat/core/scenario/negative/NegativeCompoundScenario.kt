package ru.ifmo.fbsat.core.scenario.negative

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.CompoundScenario
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.stringify
import ru.ifmo.fbsat.core.utils.toImmutable
import ru.ifmo.fbsat.core.utils.toMultiArray

class NegativeCompoundScenario private constructor(
    override val elements: List<CompoundScenarioElement>,
    override val modular: ImmutableMultiArray<NegativeScenario>
) : CompoundScenario {
    // TODO: constructor(modularNegativeScenario: MultiArray<NegativeScenario>)

    constructor(M: Int, elements: List<CompoundScenarioElement>, loopPosition: Int?) : this(
        elements = elements,
        modular = MultiArray.create(M) { (m) ->
            NegativeScenario(elements.map { it.modular.toMultiArray()[m] }, loopPosition)
        }
    )

    @Deprecated("This constructor will be removed after the ImmutableMultiArray is stabilized")
    constructor(elements: List<CompoundScenarioElement>, modular: MultiArray<NegativeScenario>) :
        this(elements, modular.toImmutable())

    override fun toString(): String = stringify()
}
