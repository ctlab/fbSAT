package ru.ifmo.fbsat.core.scenario.positive

import com.github.lipen.multiarray.MultiArray
import ru.ifmo.fbsat.core.scenario.CompoundScenario
import ru.ifmo.fbsat.core.scenario.CompoundScenarioElement
import ru.ifmo.fbsat.core.scenario.InputAction
import ru.ifmo.fbsat.core.scenario.InputEvent
import ru.ifmo.fbsat.core.scenario.InputValues
import ru.ifmo.fbsat.core.scenario.OutputAction
import ru.ifmo.fbsat.core.scenario.OutputEvent
import ru.ifmo.fbsat.core.scenario.OutputValues
import ru.ifmo.fbsat.core.scenario.ScenarioElement
import ru.ifmo.fbsat.core.scenario.negative.THE_Counterexample
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.toImmutable
import ru.ifmo.fbsat.core.utils.toMultiArray

class PositiveCompoundScenario private constructor(
    override val M: Int,
    override val elements: List<CompoundScenarioElement>,
    override val modular: ImmutableMultiArray<PositiveScenario>,
) : CompoundScenario<PositiveScenario>, CompoundImpl<PositiveScenario>() {
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

    companion object {
        fun fromCounterexample(
            counterexample: THE_Counterexample,
            M: Int,
            modularName: MultiArray<String>,
            modularInputEvents: MultiArray<List<InputEvent>>,
            modularOutputEvents: MultiArray<List<OutputEvent>>,
            modularInputNames: MultiArray<List<String>>,
            modularOutputNames: MultiArray<List<String>>,
        ): PositiveCompoundScenario {
            val ceElements = counterexample.nodes
                .map { node ->
                    node.states.single().values.associate { value ->
                        value.variable to value.content.toBoolean()
                    }
                }
                .zipWithNext { inputData, outputData ->
                    CompoundScenarioElement(
                        modular = MultiArray.create(M) { (m) ->
                            ScenarioElement(
                                inputAction = InputAction(
                                    event = modularInputEvents[m].firstOrNull {
                                        inputData.getValue("${modularName[m]}$${it.name}")
                                    },
                                    values = InputValues(modularInputNames[m].map {
                                        inputData.getValue("${modularName[m]}$$it")
                                    })
                                ),
                                outputAction = OutputAction(
                                    event = modularOutputEvents[m].firstOrNull {
                                        outputData.getValue("${modularName[m]}.${it.name}")
                                    },
                                    values = OutputValues(modularOutputNames[m].map {
                                        outputData.getValue("${modularName[m]}.$it")
                                    })
                                )
                            )
                        }
                    )
                }
            return PositiveCompoundScenario(M, ceElements)
        }
    }
}
