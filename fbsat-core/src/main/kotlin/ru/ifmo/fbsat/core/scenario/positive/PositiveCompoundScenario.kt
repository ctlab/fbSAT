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
import ru.ifmo.fbsat.core.scenario.negative.CounterexampleXML
import ru.ifmo.fbsat.core.utils.CompoundImpl
import ru.ifmo.fbsat.core.utils.project

class PositiveCompoundScenario private constructor(
    override val M: Int,
    override val elements: List<CompoundScenarioElement>,
    override val modular: MultiArray<PositiveScenario>,
) : CompoundScenario<PositiveScenario>, CompoundImpl<PositiveScenario>() {
    // TODO: constructor(modularPositiveScenario: MultiArray<PositiveScenario>)

    constructor(M: Int, elements: List<CompoundScenarioElement>) : this(
        M = M,
        elements = elements,
        modular = MultiArray.new(M) { (m) ->
            PositiveScenario(elements.map { it.project(m) })
        }
    )

    // Note: `constructor(modular: MultiArray<PositiveScenario>)` is prohibited
    // because modular scenarios might be of different sizes and couldn't be zip'ped

    companion object {
        fun fromCounterexample(
            counterexample: CounterexampleXML,
            M: Int,
            modularModuleName: MultiArray<String>,
            modularInputEvents: MultiArray<List<InputEvent>>,
            modularOutputEvents: MultiArray<List<OutputEvent>>,
            modularInputNames: MultiArray<List<String>>,
            modularOutputNames: MultiArray<List<String>>,
        ): PositiveCompoundScenario {
            val ceElements = counterexample.nodes
                .map { node ->
                    node.state.values.associate { value ->
                        value.variable to value.content.toBoolean()
                    }
                }
                .zipWithNext { inputData, outputData ->
                    CompoundScenarioElement(
                        modular = MultiArray.new(M) { (m) ->
                            ScenarioElement(
                                inputAction = InputAction(
                                    event = modularInputEvents[m].firstOrNull {
                                        inputData.getValue("${modularModuleName[m]}$${it.name}")
                                    },
                                    values = InputValues(modularInputNames[m].map {
                                        inputData.getValue("${modularModuleName[m]}$$it")
                                    })
                                ),
                                outputAction = OutputAction(
                                    event = modularOutputEvents[m].firstOrNull {
                                        outputData.getValue("${modularModuleName[m]}.${it.name}")
                                    },
                                    values = OutputValues(modularOutputNames[m].map {
                                        outputData.getValue("${modularModuleName[m]}.$it")
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
