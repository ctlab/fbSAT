package ru.ifmo.fbsat.core.scenario

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.automaton.InputEvent
import ru.ifmo.fbsat.core.automaton.InputValues
import ru.ifmo.fbsat.core.automaton.OutputEvent
import ru.ifmo.fbsat.core.automaton.OutputValues
import ru.ifmo.fbsat.core.scenario.negative.NegativeScenario
import ru.ifmo.fbsat.core.scenario.positive.PositiveScenario
import ru.ifmo.fbsat.core.utils.Compound
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.toImmutable
import ru.ifmo.fbsat.core.utils.toMultiArray

interface CompoundScenario :
    GenericScenario<CompoundScenarioElement>,
    Compound<GenericScenario<ScenarioElement>>

val CompoundScenario.modularInputActions: List<MultiArray<InputAction>>
    get() = elements.map { it.modularInputAction }
val CompoundScenario.modularInputActionsSeq: Sequence<MultiArray<InputAction>>
    get() = elements.asSequence().map { it.modularInputAction }

data class CompoundScenarioElement(
    override val modular: ImmutableMultiArray<ScenarioElement>
) : GenericScenario.Element, Compound<ScenarioElement> {
    val modularInputAction: MultiArray<InputAction> = modular.toMultiArray().map { it.inputAction }
    val modularOutputAction: MultiArray<OutputAction> = modular.toMultiArray().map { it.outputAction }
    val modularInputEvent: MultiArray<InputEvent?> = modularInputAction.map { it.event }
    val modularInputValues: MultiArray<InputValues> = modularInputAction.map { it.values }
    val modularOutputEvent: MultiArray<OutputEvent?> = modularOutputAction.map { it.event }
    val modularOutputValues: MultiArray<OutputValues> = modularOutputAction.map { it.values }

    // TODO: remove when ImmutableMultiArray is stabilized
    constructor(modular: MultiArray<ScenarioElement>) : this(modular.toImmutable())

    override fun toString(): String {
        return "CompoundElement(${modular.toMultiArray().values})"
    }
}

class PositiveCompoundScenario private constructor(
    override val elements: List<CompoundScenarioElement>,
    override val modular: ImmutableMultiArray<PositiveScenario>
) : CompoundScenario {
    // TODO: constructor(modularPositiveScenario: MultiArray<PositiveScenario>)

    constructor(M: Int, elements: List<CompoundScenarioElement>) : this(
        elements = elements,
        modular = MultiArray.create(M) { (m) ->
            PositiveScenario(elements.map { it.modular[m] })
        }
    )

    // TODO: remove when ImmutableMultiArray is stabilized
    constructor(elements: List<CompoundScenarioElement>, modular: MultiArray<PositiveScenario>) :
        this(elements, modular.toImmutable())
}

class NegativeCompoundScenario private constructor(
    override val elements: List<CompoundScenarioElement>,
    override val modular: ImmutableMultiArray<NegativeScenario>
) : CompoundScenario {
    // TODO: constructor(modularNegativeScenario: MultiArray<NegativeScenario>)

    constructor(M: Int, elements: List<CompoundScenarioElement>, loopPosition: Int?) : this(
        elements = elements,
        modular = MultiArray.create(M) { (m) ->
            NegativeScenario(elements.map { it.modular[m] }, loopPosition)
        }
    )

    // TODO: remove when ImmutableMultiArray is stabilized
    constructor(elements: List<CompoundScenarioElement>, modular: MultiArray<NegativeScenario>) :
        this(elements, modular.toImmutable())
}
