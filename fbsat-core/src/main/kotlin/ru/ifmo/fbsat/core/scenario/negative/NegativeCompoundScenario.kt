package ru.ifmo.fbsat.core.scenario.negative

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
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.ImmutableMultiArray
import ru.ifmo.fbsat.core.utils.project
import ru.ifmo.fbsat.core.utils.toImmutable

class NegativeCompoundScenario private constructor(
    override val M: Int,
    override val elements: List<CompoundScenarioElement>,
    val loopPosition: Int?,
    override val modular: ImmutableMultiArray<NegativeScenario>,
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

    companion object {
        fun fromCounterexample(
            counterexample: THE_Counterexample,
            M: Int,
            modularName: MultiArray<String>,
            modularInputEvents: MultiArray<List<InputEvent>>,
            modularOutputEvents: MultiArray<List<OutputEvent>>,
            modularInputNames: MultiArray<List<String>>,
            modularOutputNames: MultiArray<List<String>>,
        ): NegativeCompoundScenario {
            val loopPosition = counterexample.loops.trim().split(" ").firstOrNull()?.toInt()
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
            return NegativeCompoundScenario(M, ceElements, loopPosition?.let { it - 1 })
        }
    }
}
