package ru.ifmo.fbsat.core.scenario

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.utils.Compound
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.toImmutable
import ru.ifmo.fbsat.core.utils.toMultiArray

interface CompoundScenario : GenericScenario<CompoundScenarioElement>, Compound<Scenario>

// val CompoundScenario.modularInputActions: List<MultiArray<InputAction>>
//     get() = elements.map { it.modularInputAction }
// val CompoundScenario.modularInputActions: List<MultiArray<InputAction>>
//     get() = inputActions.map { it.modular }
@Deprecated(
    "Use inputActionSeq and map it manually",
    ReplaceWith("inputActionsSeq.map { it.modular }")
)
val CompoundScenario.modularInputActionsSeq: Sequence<MultiArray<InputAction>>
    get() = inputActionsSeq.map { it.modular }

// TODO: fix the constructor
data class CompoundScenarioElement(
    override val modular: ImmutableMultiArray<ScenarioElement>
) : GenericScenario.Element<CompoundInputAction, CompoundOutputAction>,
    Compound<ScenarioElement> {

    val modularInputAction: MultiArray<InputAction> = modular.toMultiArray().map { it.inputAction }
    val modularOutputAction: MultiArray<OutputAction> = modular.toMultiArray().map { it.outputAction }
    val modularInputEvent: MultiArray<InputEvent?> = modularInputAction.map { it.event }
    val modularInputValues: MultiArray<InputValues> = modularInputAction.map { it.values }
    val modularOutputEvent: MultiArray<OutputEvent?> = modularOutputAction.map { it.event }
    val modularOutputValues: MultiArray<OutputValues> = modularOutputAction.map { it.values }

    override val inputAction: CompoundInputAction =
        CompoundInputAction(modularInputAction.toImmutable())
    override val outputAction: CompoundOutputAction =
        CompoundOutputAction(modularOutputAction.toImmutable())

    @Deprecated("This constructor will be removed after the ImmutableMultiArray is stabilized")
    constructor(modular: MultiArray<ScenarioElement>) : this(modular.toImmutable())

    override fun toString(): String {
        return "CompoundElement(${modular.toMultiArray().values})"
    }
}
