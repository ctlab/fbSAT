package ru.ifmo.fbsat.core.scenario

import com.github.lipen.multiarray.MultiArray
import com.github.lipen.multiarray.map
import ru.ifmo.fbsat.core.utils.Compound
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.project
import ru.ifmo.fbsat.core.utils.toImmutable
import ru.ifmo.fbsat.core.utils.toMultiArray

interface CompoundScenario<S> : GenericScenario<CompoundScenarioElement>, Compound<S>
    where S : Scenario

// val CompoundScenario.modularInputActions: List<MultiArray<InputAction>>
//     get() = elements.map { it.modularInputAction }
// val CompoundScenario.modularInputActions: List<MultiArray<InputAction>>
//     get() = inputActions.map { it.modular }
@Deprecated(
    "Use inputActionSeq and map it manually",
    ReplaceWith("inputActionsSeq.map { it.modular }")
)
val CompoundScenario<*>.modularInputActionsSeq: Sequence<MultiArray<InputAction>>
    get() = inputActionsSeq.map { it.modular }

class CompoundScenarioElement private constructor(
    override val M: Int,
    override val inputAction: CompoundInputAction,
    override val outputAction: CompoundOutputAction,
    override val modular: ImmutableMultiArray<ScenarioElement>
) : GenericScenario.Element<CompoundInputAction, CompoundOutputAction>,
    CompoundImpl<ScenarioElement>() {

    var nodeId: Int? = null

    val modularInputAction: MultiArray<InputAction> = inputAction.modular
    val modularOutputAction: MultiArray<OutputAction> = outputAction.modular
    val modularInputEvent: MultiArray<InputEvent?> = modularInputAction.map { it.event }
    val modularInputValues: MultiArray<InputValues> = modularInputAction.map { it.values }
    val modularOutputEvent: MultiArray<OutputEvent?> = modularOutputAction.map { it.event }
    val modularOutputValues: MultiArray<OutputValues> = modularOutputAction.map { it.values }

    constructor(
        M: Int,
        inputAction: CompoundInputAction,
        outputAction: CompoundOutputAction
    ) : this(
        M = M,
        inputAction = inputAction,
        outputAction = outputAction,
        modular = MultiArray.create(M) { (m) ->
            ScenarioElement(
                inputAction = inputAction.project(m),
                outputAction = outputAction.project(m)
            )
        }.toImmutable()
    )

    constructor(modular: MultiArray<ScenarioElement>) : this(
        M = modular.shape.single(),
        inputAction = CompoundInputAction(modular.map { it.inputAction }),
        outputAction = CompoundOutputAction(modular.map { it.outputAction }),
        modular = modular.toImmutable()
    )

    override fun toString(): String {
        return "CompoundElement(${modular.toMultiArray().values})"
    }
}
